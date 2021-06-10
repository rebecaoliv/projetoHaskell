{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Handler.Auxiliar

formLogin :: Form (User, Text)
formLogin = renderDivs $ (,) 
    <$> (User 
        <$> areq textField "E-mail: "  Nothing 
        <*> areq passwordField "Senha:  "  Nothing
        )
    <*> areq passwordField  "Cofirmacao: " Nothing

getUserR :: Handler Html
getUserR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout (formWidget widget msg UserR "Cadastrar")

postUserR :: Handler Html
postUserR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (user@(User email senha), conf) -> do
            userExiste <- runDB $ getBy (UniqueEmail email)
            case userExiste of
                    Just _ -> do
                        setMessage [shamlet|
                                <div> 
                                    E-MAIL JA CADASTRADO!
                            |]
                        redirect UserR
                    Nothing -> do
                        if senha == conf then do
                            runDB $ insert user
                            setMessage [shamlet|
                                <div>
                                    USUARIO INSERIDO COM SUCESSO!
                            |]
                            redirect UserR
                        else do
                            setMessage [shamlet|
                                <div>
                                    SENHA E CONFIRMACAO DIFERENTES!
                            |]
                            redirect UserR
        _ -> redirect HomeR
