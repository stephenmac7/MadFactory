module Handler.Play where

import           Import
import           MadLibUtil
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc

getPlayR :: MadLibId -> Handler Html
getPlayR libId = do
    -- DB Ops
    lib <- runDB $ get404 libId
    -- Create form
    let lws = sortWords . extractLibs . readHtml def . renderHtml $ madLibContent lib
        showError = False
    (widget, enctype) <- generateFormPost $ madContentsForm lws
    -- Set up the layout
    defaultLayout $ do
        setTitleI $ madLibTitle lib
        $(widgetFile "play")

postPlayR :: MadLibId -> Handler Html
postPlayR libId = do
    -- DB Ops
    lib <- runDB $ get404 libId
    -- Parse that form
    let pandoced = readHtml def . renderHtml $ madLibContent lib
    ((res, widget), enctype) <- runFormPost . madContentsForm . sortWords $ extractLibs pandoced
    -- Do something with it
    let title = madLibTitle lib
    defaultLayout $ do
        setTitleI title
        case res of FormSuccess mcontents -> let results = generateOutput pandoced mcontents in $(widgetFile "results")
                    _                     -> let showError = True in $(widgetFile "play")
