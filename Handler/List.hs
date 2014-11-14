module Handler.List where

import           Control.Monad         (liftM)
import           Import
import           Yesod.Form.Bootstrap3
import           CodeMirror

hForm :: BootstrapFormLayout
hForm = BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 10)

submit :: AppMessage -> AForm Handler ()
submit m = bootstrapSubmit (BootstrapSubmit m "btn-default" [])

madLibForm :: Form MadLib
madLibForm = renderBootstrap3 hForm $ MadLib
    <$> areq textField (bfs MsgNewMadLibTitle) (Just "No Title")
    <*> lift requireAuthId
    <*> lift (liftIO getCurrentTime)
    <*> pure False
    <*> areq htmlField (bfs MsgNewMadLibContents) Nothing
    <*  submit MsgNewMadLibSubmit

getListR :: Handler Html
getListR = do
    mauth <- maybeAuth
    formMaybe <- case mauth of
                   Just _  -> liftM Just $ generateFormPost madLibForm
                   Nothing -> return Nothing
    let filters = let baseFil = [MadLibPublic ==. True] in
                    case mauth of
                      Just (Entity userId user)  -> if userAdmin user then [] else baseFil ||. [MadLibUser ==. userId]
                      Nothing                    -> baseFil
    libs <- runDB $ selectList filters [Desc MadLibAdded]
    defaultLayout $ do
        setTitleI MsgListTitle
        codeMirror
        $(widgetFile "list")

postListR :: Handler Html
postListR = do
    ((res, widget), enctype) <- runFormPost madLibForm
    case res of
        FormSuccess lib -> do
            libId <- runDB $ insert lib
            setMessageI $ MsgMadLibCreated $ madLibTitle lib
            redirect $ MadLibR libId
        _ -> defaultLayout $ do
            setTitleI MsgListTitle
            codeMirror
            $(widgetFile "newLibFail")
