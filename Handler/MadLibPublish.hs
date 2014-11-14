module Handler.MadLibPublish where

import Import

postMadLibPublishR :: MadLibId -> Handler Value
postMadLibPublishR libId = do
    public <- runInputPost $ ireq boolField "public"
    runDB $ update libId [MadLibPublic =. public]
    return $ object []
