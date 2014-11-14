module Handler.MadLib where

import           CodeMirror
import           Control.Monad         (when)
import           Handler.List          (hForm, submit)
import           Import
import           Text.Julius           (rawJS)
import           Yesod.Form.Bootstrap3

-- Forms
madLibEditForm :: MadLib -> Form (Text, Html)
madLibEditForm lib = renderBootstrap3 hForm $ (,)
    <$> areq textField (bfs MsgNewMadLibTitle) (Just $ madLibTitle lib)
    <*> areq htmlField (bfs MsgNewMadLibContents) (Just $ madLibContent lib)
    <*  submit MsgEditMadLibSubmit

-- Widgets
bootstrapSwitch :: Text -> [(Text, Text)] -> Widget
bootstrapSwitch htmlId attrs = do
    addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/bootstrap-switch/3.2.1/css/bootstrap3/bootstrap-switch.min.css"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/bootstrap-switch/3.2.1/js/bootstrap-switch.min.js"
    [whamlet|
        <input type="checkbox" id=#{htmlId} *{attrs}>
    |]

-- Routes
getMadLibR :: MadLibId -> Handler Html
getMadLibR libId = do
    -- DB Ops
    (lib, ownerEmail) <- runDB $ do
        lib <- get404 libId
        owner <- get $ madLibUser lib
        return (lib, fmap userIdent owner)
    -- Figure out user information
    mauth <- maybeAuth
    let ownerId = madLibUser lib
    let perm = case mauth of
                 Nothing -> False
                 Just (Entity userId user) -> userAdmin user || ownerId == userId
    -- Set up the layout
    switchId <- newIdent
    defaultLayout $ do
        when perm codeMirror
        setTitleI $ madLibTitle lib
        $(widgetFile "madLib")

postMadLibR :: MadLibId -> Handler Html
postMadLibR libId = do
    lib <- runDB $ get404 libId
    ((res, widget), enctype) <- runFormPostNoToken $ madLibEditForm lib
    case res of
      FormSuccess (title, content) -> do
          runDB $ update libId [MadLibTitle =. title, MadLibContent =. content]
          setMessageI . MsgMadLibUpdated $ madLibTitle lib
          redirect $ MadLibR libId
      _ -> defaultLayout $ do
          setTitleI $ madLibTitle lib
          codeMirror
          $(widgetFile "editLibFail")

deleteMadLibR :: MadLibId -> Handler Value
deleteMadLibR libId = do
    lib <- runDB $ do
        lib <- get404 libId
        delete libId
        return lib
    setMessageI $ MsgMadLibDeleted $ madLibTitle lib
    return $ object []

