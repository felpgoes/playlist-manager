{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Text.Cassius
import Handler.Auxiliar

formLogin :: Form (User, Text)
formLogin = renderDivs $ (,) 
    <$> (User
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Password: " Nothing
        )
    <*> areq passwordField "Confirmação: " Nothing

getUserR :: Handler Html
getUserR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout (formWidget widget msg UserR "Cadastrar")

postUserR :: Handler Html
postUserR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (user@(User email password), conf) -> do
            if password == conf then do
                runDB $ insert user
                setMessage [shamlet|
                    <div>
                        Usuário inserido com sucesso!
                |]
                redirect UserR
            else do
                setMessage [shamlet|
                    <div>
                        Password e confirmação diferentes!
                |]
                redirect UserR
        _ -> redirect HomeR