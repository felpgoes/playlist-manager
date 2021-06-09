{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Cassius

formLogin :: Form User
formLogin = renderDivs $ User
    <$> areq textField "E-mail: " Nothing
    <*> areq passwordField "Password: " Nothing

getAuthUserR :: Handler Html
getAuthUserR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    user <- lookupSession "_ID"
    defaultLayout $ do
        setTitle "Login"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/Login.cassius")
        (formWidget widget msg AuthUserR "Entrar")

postAuthUserR :: Handler Html
postAuthUserR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (User email password) -> do 
           userExiste <- runDB $ getBy (UniqueEmail email)
           case userExiste of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div>
                            User não encontrado!
                    |]
                    redirect AuthUserR
                Just (Entity _ user) -> do 
                    if password == userPassword user then do
                        setSession "_ID" (userEmail user)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <div>
                                Password INCORRETA!
                        |]
                        redirect AuthUserR
        _ -> redirect HomeR

postLogoutUserR :: Handler Html
postLogoutUserR = do
    deleteSession "_ID"
    redirect HomeR

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg route mensagem = $(whamletFile "templates/Login.hamlet")
