module Handler.MadLib where

import Import

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
    let perm = case mauth of Nothing -> False
                             (Just (Entity userId user)) -> userAdmin user || ownerId == userId
    -- Set up the layout
    defaultLayout $ do
        setTitleI $ madLibTitle lib
        $(widgetFile "madLib")

postMadLibR :: MadLibId -> Handler Html
postMadLibR = error "postMadLibR not yet implemented."

deleteMadLibR :: MadLibId -> Handler Value
deleteMadLibR libId = do
    lib <- runDB $ do
        lib <- get404 libId
        delete libId
        return lib
    setMessageI $ MsgMadLibDeleted $ madLibTitle lib
    return $ object []

