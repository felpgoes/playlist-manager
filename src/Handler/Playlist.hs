{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Playlist where

import Import
import Text.Cassius
import Database.Persist.Postgresql

formPlaylist :: UserId -> Form Playlist
formPlaylist uid = renderDivs $ Playlist
    <$> pure uid
    <*> areq textField "Nome: " Nothing
    <*> areq textField "Descrição: " Nothing
    <*> areq (selectField playlistsCB) "Musica: " Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))

playlistsCB :: HandlerFor App (OptionList (Key Music))
playlistsCB = do
    musics <- runDB $ selectList [] [Asc MusicName]
    optionsPairs $
        map (\r -> (musicName $ entityVal r, entityKey r)) musics

getPlaylistR :: UserId -> Handler Html
getPlaylistR uid = do
    (widget,_) <- generateFormPost (formPlaylist uid)
    msg <- getMessage
    defaultLayout $ do
        setTitle "Editar musica"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        (formWidget widget msg (PlaylistR uid) "Criar") 

postPlaylistR :: UserId -> Handler Html
postPlaylistR uid = do
    ((result,_),_) <- runFormPost (formPlaylist uid)
    case result of
        FormSuccess playlist -> do
            _ <- runDB $ insert playlist
            setMessage [shamlet|
                <div>
                    Playlist feita com sucesso!
            |]
            redirect (PlaylistR uid)
        _ -> redirect HomeR

getPlaylistsR :: Handler Html
getPlaylistsR = do 
    let sql = "SELECT ??,??,?? FROM playlist \
          \ INNER JOIN \"user\" ON \"user\".id = playlist.user_id \
          \ INNER JOIN music ON music.id = playlist.music_id \
          \ WHERE \"user\".email = ?"
    user <- lookupSession "_ID"
    playlists <- runDB $ rawSql sql [toPersistValue user] :: Handler [(Entity Playlist, Entity User, Entity Music)]
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/PlaylistList.cassius")
        $(whamletFile "templates/PlaylistList.hamlet") 


formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg route mensagem = $(whamletFile "templates/DefaultForm.hamlet")