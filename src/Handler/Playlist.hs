{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Playlist where

import Import
import Text.Cassius
import Text.Julius

formPlaylist :: Form Playlist
formPlaylist = renderDivs $ Playlist
    <$> areq textField "Nome "        Nothing
    <*> areq textField "Descrição  "  Nothing
    <*> areq textField "Apelido "     Nothing

getPlaylistR :: PlaylistId -> Handler Html
getPlaylistR playId = do
    playlist <- runDB $ get404 playId
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/Playlist.cassius")
        $(whamletFile "templates/Playlist.hamlet")

getPlaylistFormR :: Handler Html
getPlaylistFormR = do   
    (widget,_) <- generateFormPost formPlaylist
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/PlaylistForm.cassius")
        $(whamletFile "templates/PlaylistForm.hamlet")

postCriarPlaylistR :: Handler Html
postCriarPlaylistR = do
    ((result,_),_) <- runFormPost formPlaylist
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
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/PlaylistList.cassius")
        $(whamletFile "templates/PlaylistList.hamlet")