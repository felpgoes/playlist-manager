{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Music where

import Import
import Text.Cassius

formMusic :: Maybe Music -> Form Music
formMusic msc = renderDivs $ Music
    <$> areq textField "Nome "        (fmap musicName msc)
    <*> areq textField "Album  "  (fmap musicAlbum msc)
    <*> areq textField "Autor  "  (fmap musicAutor msc)
    <*> areq textField "Url "     (fmap musicExternalUrl msc)

getMusicR :: MusicId -> Handler Html
getMusicR mscId = do
    music <- runDB $ get404 mscId
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/Music.cassius")
        $(whamletFile "templates/Music.hamlet")

getMusicFormR :: Handler Html
getMusicFormR = do   
    (widget,_) <- generateFormPost (formMusic Nothing)
    msg <- getMessage
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/MusicForm.cassius")
        $(whamletFile "templates/MusicForm.hamlet")

postCriarMusicR :: Handler Html
postCriarMusicR = do
    ((result,_),_) <- runFormPost (formMusic Nothing)
    case result of
        FormSuccess music -> do
            runDB $ insert music
            setMessage [shamlet|
                <div>
                    Music #{musicName music} foi criada com sucesso!
            |]
            redirect ListMusicsR
        _ -> redirect HomeR

postDelMusicR :: MusicId -> Handler Html
postDelMusicR mscId = do
    runDB $ delete mscId
    redirect ListMusicsR

getListMusicsR :: Handler Html
getListMusicsR = do
    musics <- runDB $ selectList [] [Desc MusicId]
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/MusicList.cassius")
        $(whamletFile "templates/MusicList.hamlet")

getEditMusicR :: MusicId -> Handler Html
getEditMusicR mscId = do
    music <- runDB $ get404 mscId
    user <- lookupSession "_ID"
    (widget, _) <- generateFormPost (formMusic (Just music))
    msg <- getMessage
    defaultLayout $ do
        setTitle "Editar musica"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        (formWidget widget msg (EditMusicR mscId) "Editar") 

postEditMusicR :: MusicId -> Handler Html
postEditMusicR mscId = do
    oldMusic <- runDB $ get404 mscId
    ((result,_),_) <- runFormPost (formMusic Nothing)
    case result of
        FormSuccess newMusic -> do
            runDB $ replace mscId newMusic
            redirect ListMusicsR
        _ -> redirect HomeR

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg route mensagem = $(whamletFile "templates/DefaultForm.hamlet")