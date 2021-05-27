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
    <$> areq textField "Nome: "        Nothing
    <*> areq textField "Descrição:  "  Nothing
    <*> areq textField  "id: "          Nothing

getPlaylistR :: PlaylistId -> Handler Html
getPlaylistR ident = do
    defaultLayout $ do
        setTitle "getPlaylistR"
    -- playlist <- runDB $ get404 ident
    -- defaultLayout $ do
    --     addStylesheet (StaticR css_bootstrap_css)
    --     toWidgetHead $(cassiusFile "templates/Padrao.cassius")
    --     $(whamletFile "templates/Cliente.hamlet")

postCriarPlaylistR :: Handler Html
postCriarPlaylistR = do
    ((result,_),_) <- runFormPost formPlaylist
    case result of
        FormSuccess playlist -> do
            runDB $ insert playlist
            setMessage [shamlet|
                <div>
                    A playlist #{playlistName playlist} foi criada!
            |]
            redirect ListPlaylistsR
        _ -> redirect HomeR

postDelPlaylistR :: PlaylistId -> Handler Html
postDelPlaylistR id = do
    runDB $ delete id
    redirect HomeR

getListPlaylistsR :: Handler Html
getListPlaylistsR = do
    defaultLayout $ do
        setTitle "getListPlaylistsR"