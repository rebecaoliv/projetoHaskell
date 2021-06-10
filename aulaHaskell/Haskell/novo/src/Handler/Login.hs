{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Handler.Auxiliar

formLogin :: Form User
formLogin = renderDivs $ User 
    <$> areq textField "E-mail: "  Nothing 
    <*> areq passwordField "Senha:  "  Nothing


getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout (formWidget widget msg AutR "Entrar")


--Bcrypt
postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (User "root@root.com" "root") -> do
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (User email senha) -> do
            userExiste <- runDB $ getBy (UniqueEmail email)
            case userExiste of
                    Nothing -> do
                        setMessage [shamlet|
                            Usuario nao cadastrado
                        |]
                        redirect AutR
                    Just (Entity _ user) -> do
                        if senha == userSenha user then do
                            setSession "_ID" (userEmail user)
                            redirect HomeR
                        else do
                            setMessage [shamlet|
                                User e/ou senha nao conferem
                            |]
                            redirect AutR
        _-> redirect HomeR

-- Session => chave/valor
-- No nosso caso, chave = _ID, valor = email
postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = do
    redirect HomeR