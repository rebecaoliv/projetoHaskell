{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Cassius

--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

--getHomeR :: Handler Html
--getHomeR = undefined

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg rota m = $(whamletFile "templates/form.hamlet")

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        user <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/home.cassius")
        $(whamletFile "templates/bootstrap.hamlet")
        -- $(whamletFile "templates/navbar.hamlet") 
        $(whamletFile "templates/home.hamlet") 

        
        