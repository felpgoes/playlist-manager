{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Music where

import Import
import Text.Cassius
import Text.Julius

getMusicR :: PlaylistId -> Handler Html
getMusicR pid = do
    defaultLayout $ do
        setTitle "getFindMusicR"

getFindMusicR :: PlaylistId -> MusicId -> Handler Html
getFindMusicR pid mid = do
    defaultLayout $ do
        setTitle "getMusicR"

postDelMusicR :: PlaylistId -> MusicId -> Handler Html
postDelMusicR pid mid = do
    runDB $ delete mid
    redirect HomeR