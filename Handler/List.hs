module Handler.List where

import           Control.Monad         (liftM)
import           Import
import           Yesod.Form.Bootstrap3

madLibForm :: Form MadLib
madLibForm = renderBootstrap3 (BootstrapHorizontalForm (ColMd 0) (ColMd 2) (ColMd 0) (ColMd 10)) $ MadLib
    <$> areq textField (bfs MsgNewMadLibTitle) (Just "No Title")
    <*> lift requireAuthId
    <*> lift (liftIO getCurrentTime)
    <*> areq htmlField (bfs MsgNewMadLibContents) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgNewMadLibSubmit "btn-default" [])

getListR :: Handler Html
getListR = do
    mauth <- maybeAuth
    formMaybe <- case mauth of
                   Nothing -> return Nothing
                   Just _  -> liftM Just $ generateFormPost madLibForm
    libs <- runDB $ selectList [] [Desc MadLibAdded]
    defaultLayout $ do
        setTitleI MsgListTitle
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
            $(widgetFile "addFail")
