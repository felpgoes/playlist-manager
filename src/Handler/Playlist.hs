{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Playlist where

import Import
import Text.Cassius

formPlaylist :: Maybe Playlist -> Form Playlist
formPlaylist play = renderDivs $ Playlist
    <$> areq textField "Nome "        (fmap playlistName play)
    <*> areq textField "Descrição  "  (fmap playlistDescription play)
    <*> areq textField "Url "     (fmap playlistExternalUrl play)

getPlaylistR :: PlaylistId -> Handler Html
getPlaylistR playId = do
    playlist <- runDB $ get404 playId
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/Playlist.cassius")
        $(whamletFile "templates/Playlist.hamlet")

getPlaylistFormR :: Handler Html
getPlaylistFormR = do   
    (widget,_) <- generateFormPost (formPlaylist Nothing)
    msg <- getMessage
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/PlaylistForm.cassius")
        $(whamletFile "templates/PlaylistForm.hamlet")

postCriarPlaylistR :: Handler Html
postCriarPlaylistR = do
    ((result,_),_) <- runFormPost (formPlaylist Nothing)
    case result of
        FormSuccess playlist -> do
            runDB $ insert playlist
            setMessage [shamlet|
                <div>
                    Playlist #{playlistName playlist} foi criada com sucesso!
            |]
            redirect ListPlaylistsR
        _ -> redirect HomeR

postDelPlaylistR :: PlaylistId -> Handler Html
postDelPlaylistR playId = do
    runDB $ delete playId
    redirect ListPlaylistsR

getListPlaylistsR :: Handler Html
getListPlaylistsR = do
    playlists <- runDB $ selectList [] [Desc PlaylistId]
    user <- lookupSession "_ID"
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/PlaylistList.cassius")
        $(whamletFile "templates/PlaylistList.hamlet")

getEditPlaylistR :: PlaylistId -> Handler Html
getEditPlaylistR playId = do
    playlist <- runDB $ get404 playId
    user <- lookupSession "_ID"
    (widget, _) <- generateFormPost (formPlaylist (Just playlist))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditPlaylistR playId) "Editar")

postEditPlaylistR :: PlaylistId -> Handler Html
postEditPlaylistR playId = do
    oldPlaylist <- runDB $ get404 playId
    ((result,_),_) <- runFormPost (formPlaylist Nothing)
    case result of
        FormSuccess newPlaylist -> do
            runDB $ replace playId newPlaylist
            redirect ListPlaylistsR
        _ -> redirect HomeR

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg route mensagem = $(whamletFile "templates/DefaultForm.hamlet")