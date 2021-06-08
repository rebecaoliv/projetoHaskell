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
getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 1
        |]

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |]

getPage3R :: Handler Html
getPage3R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 3
        |]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        --estatico
        --toWidgetHead $(juliusFile "templates/home.julius")
        --css/bootstrap.css
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/home.cassius")
        $(whamletFile "templates/home.hamlet") 
        