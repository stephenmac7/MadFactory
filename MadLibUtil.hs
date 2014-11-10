module MadLibUtil where

import Import
import Text.Pandoc.Walk (query, walkM)
import Data.Text (append, pack, unpack)
import Data.Attoparsec.Text
import Text.Pandoc
import qualified Data.Attoparsec.Text as APT
import qualified Control.Monad.State.Lazy as ST
import Yesod.Form.Bootstrap3
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
      view idAttr nameAttr otherAttrs eResult isReq = [whamlet|
        <div .row>
            <div .form-horizontal>
                $forall (single, val) <- pairs
                    <div .form-group>
                        <label .col-xs-offset-1 .col-xs-3 .control-label>#{single}
                        <div .col-xs-8>
                            <input id="#{idAttr}-#{single}" name=#{nameAttr} value=#{val} *{otherAttrs} :isReq:required>
      |]
        where
          pairs = case eResult of Right vals -> paddedZip singles vals
                                  Left _ -> paddedZip singles []

multiesField :: [Text] -> Field Handler (Map.Map Text Text)
multiesField multies = Field p view UrlEncoded
    where
      p vals _
        | checkLength vals multies = return $ Right $ Just . Map.fromList $ zip multies vals
        | otherwise                = return $ Right Nothing
      view idAttr nameAttr otherAttrs eResult isReq = [whamlet|
        <div .row>
            <div .form-horizontal>
                $forall (multi, val) <- Map.toList results
                    <div .form-group>
                        <label .col-xs-offset-1 .col-xs-3 .control-label>#{multi}
                        <div .col-xs-8>
                            <input id="#{idAttr}-#{multi}" name=#{nameAttr} value=#{val} *{otherAttrs} :isReq:required>
      |]
        where
          results = case eResult of Right m -> m
                                    Left _ -> foldl (\acc x -> Map.insert x "" acc) Map.empty multies

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
    let parsedB = case btype of 's' -> SingleBlank
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
sortWords lws = let (singles, multies) = foldr f ([], Set.empty) lws in (singles, Set.toList multies)
    where f x (singles, multies) = case x of Word _ -> (singles, multies)
                                             SingleBlank _ pos _ -> (pos:singles, multies)
                                             MultiBlank _ pos _ -> (singles, Set.insert pos multies)

extractLib :: Inline -> [LibWord]
extractLib (Str s) = let packed = pack s in case parseOnly wordParser packed of
    Left _ -> [Word packed]
    Right x -> [x]
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
    case parseOnly wordParser (pack s) of
        Left _ -> return $ Str s
        Right x -> case x of SingleBlank begin _ end -> ST.put (MadContents nextSingles multies) >> (return . Str . unpack $ begin `append` thisSingle `append` end)
                             MultiBlank begin pos end -> return $ Str . unpack $ begin `append` Map.findWithDefault "" pos multies `append` end
                             Word _ -> return $ Str "This should never happen."
generateEach x = return x

generateOutput :: Pandoc -> MadContents -> Html
generateOutput input = writeHtml def . ST.evalState (walkM generateEach input)
