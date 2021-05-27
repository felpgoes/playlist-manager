{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Cassius
import Text.Julius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql
    
-- Interpolador @ mexe com links

-- O ideal eh ter apenas chamadas a templates.
-- css_bootstrap_css => css/bootstrap.css
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Home"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/Padrao.cassius")
        toWidgetHead $(cassiusFile "templates/Home.cassius")
        $(whamletFile "templates/Home.hamlet")