{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Temporada where
-- ----------------------------------------------------------------------------------------------------------------------
-- IMPORT
-- ----------------------------------------------------------------------------------------------------------------------
import Control.Monad
import Import 
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
-- ----------------------------------------------------------------------------------------------------------------------
-- POST
-- ----------------------------------------------------------------------------------------------------------------------
postCadTempR :: Text -> Handler Value 
postCadTempR token = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] []
    case maybeUser of 
        Just (Entity uid usuario) -> do
            temp <- requireJsonBody :: Handler Temporada
            tempid <- runDB $ insert temp
            sendStatusJSON created201 (object ["resp" .= fromSqlKey tempid])
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
-- GET
-- ----------------------------------------------------------------------------------------------------------------------
getListaTempR :: Text -> SerieId -> Handler Value
getListaTempR token seriid = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] []
    case maybeUser of 
        Just (Entity uid usuario) -> do
            qt <- runDB $ selectList [TemporadaSeriid ==. seriid] [] 
            list <- return $ fmap (\(Entity _  a) -> a) qt
            sendStatusJSON ok200 (object ["resp" .= length list])
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
-- 
-- ----------------------------------------------------------------------------------------------------------------------
