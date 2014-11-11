module MadLibUtil where

import qualified Control.Monad.State.Lazy as ST
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text     as APT
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Text                (pack, unpack)
import qualified Data.Text                as T (concat)
import           Import
import           Text.Pandoc
import           Text.Pandoc.Walk         (query, walkM)
import           Yesod.Form.Bootstrap3

-- Forms
madContentsForm :: ([Text], [Text]) -> Form MadContents
madContentsForm (singles, multies) = renderBootstrap3 BootstrapBasicForm $ MadContents
    <$> areq (singlesField singles) (bfs MsgSingles) Nothing
    <*> areq (multiesField multies) (bfs MsgMulties) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgPlaySubmit "btn-primary" [])

paddedZip :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
paddedZip [] ys = zip (repeat mempty) ys
paddedZip xs [] = zip xs (repeat mempty)
paddedZip (x:xs) (y:ys) = (x, y):paddedZip xs ys

checkLength :: (Eq a, Monoid a) => [a] -> [a] -> Bool
checkLength xs ys = trueLength == length ys
    where trueLength = length $ filter (/= mempty) xs

singlesField :: [Text] -> Field Handler [Text]
singlesField singles = Field p view UrlEncoded
    where
      p vals _
        | checkLength vals singles = return $ Right $ Just vals
        | otherwise                = return $ Right Nothing
      view = viewHelper (\x ->
               case x of
                 Right vals -> paddedZip singles vals
                 Left _ -> paddedZip singles [])

multiesField :: [Text] -> Field Handler (Map.Map Text Text)
multiesField multies = Field p view UrlEncoded
    where
      p vals _
        | checkLength vals multies = return $ Right $ Just . Map.fromList $ zip multies vals
        | otherwise                = return $ Right Nothing
      view = viewHelper (\x -> 
               case x of
                 Right m -> Map.toList m
                 Left _ -> paddedZip multies [])

viewHelper :: (Either Text a -> [(Text, Text)]) -> FieldViewFunc Handler a
viewHelper pairF idAttr nameAttr otherAttrs eResult isReq = let pairs = pairF eResult in do
    [whamlet|
      $forall (key, val) <- pairs
          <input .word .form-control id="#{idAttr}-#{key}" name=#{nameAttr} value=#{val} placeholder=#{key} *{otherAttrs} :isReq:required>
    |]
    toWidget [lucius| input.word { margin-top: 10px; }|]

-- MadLib heavy lifting
data MadContents = MadContents [Text] (Map.Map Text Text)
data LibWord = Word Text | SingleBlank Text Text Text | MultiBlank Text Text Text deriving (Show)

wordParser :: Parser LibWord
wordParser = do
    -- Begins with a [
    begin <- APT.takeWhile (/= '[')
    -- Skip the bracket
    _ <- anyChar
    -- Get the BlankType
    btype <- satisfy (\x -> x == 's' || x == 'm')
    let parsedB = case btype of
                    's' -> SingleBlank
                    'm' -> MultiBlank
                    _   -> \_ _ _ -> Word "This should never happen."
    -- Sep. from the part of speech by a colon
    _ <- char ':'
    -- The part of speech
    pos <- APT.takeWhile (/= ']')
    -- Skip the bracket
    _ <- anyChar
    -- Take the last part of the text
    end <- takeText

    return $ parsedB begin pos end

sortWords :: [LibWord] -> ([Text], [Text])
sortWords lws = let (singles, multies) = foldr f ([], Set.empty) lws
                  in (singles, Set.toList multies)
    where f x (singles, multies) =
            case x of Word _ -> (singles, multies)
                      SingleBlank _ pos _ -> (pos:singles, multies)
                      MultiBlank _ pos _ -> (singles, Set.insert pos multies)

extractLib :: Inline -> [LibWord]
extractLib (Str s) = case parseOnly wordParser packed of
    Left _ -> [Word packed]
    Right x -> [x]
  where
    packed = pack s
extractLib _ = []

extractLibs :: Pandoc -> [LibWord]
extractLibs = query extractLib

safeDetatch :: (Monoid a) => [a] -> (a, [a])
safeDetatch [] = (mempty, [])
safeDetatch (x:xs) = (x, xs)

generateEach :: Inline -> ST.State MadContents Inline
generateEach (Str s) = do
    (MadContents singles multies) <- ST.get
    let (thisSingle, nextSingles) = safeDetatch singles
        beheaded                  = MadContents nextSingles multies
    case parseOnly wordParser (pack s) of
        Left _ -> return $ Str s
        Right x -> case x of
                     SingleBlank begin _ end -> do
                         ST.put beheaded
                         return . Str . unpack $ T.concat [begin, thisSingle, end]
                     MultiBlank begin pos end -> return $ Str . unpack $ T.concat [begin, Map.findWithDefault "" pos multies, end]
                     Word _ -> return $ Str "This should never happen."
generateEach x = return x

generateOutput :: Pandoc -> MadContents -> Html
generateOutput input = writeHtml def . ST.evalState (walkM generateEach input)
