{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Serie where
-- ----------------------------------------------------------------------------------------------------------------------
-- IMPORT
-- ----------------------------------------------------------------------------------------------------------------------
import Control.Monad
import Import 
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Database.Persist.Postgresql 
import Handler.Temporada
-- ----------------------------------------------------------------------------------------------------------------------
-- POST
-- ----------------------------------------------------------------------------------------------------------------------
postCadSerieR :: Text -> Handler Value 
postCadSerieR token = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] []
    case maybeUser of 
        Just (Entity uid usuario) -> do
            serie <- requireJsonBody :: Handler Serie
            serieid <- runDB $ do
                serieExists <- getBy $ UniqueApi (serieIdApi serie)
                case serieExists of
                    Nothing -> insert serie
                    Just x  -> return $ entityKey x
            userserie <- runDB . insert $ UserSerie uid serieid
            sendStatusJSON created201 (object ["resp" .= fromSqlKey userserie])
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
-- GET
-- ----------------------------------------------------------------------------------------------------------------------
getListarSeriesR :: Text -> Handler Value 
getListarSeriesR token = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] []
    case maybeUser of 
        Just (Entity uid usuario) -> do
            seriesId <- runDB $ selectList [UserSerieUserid ==. uid] [] 
            series <- runDB $ mapM (get404 . userSerieSeriid . entityVal) seriesId 
            sendStatusJSON ok200 (object ["resp" .= (toJSON series) ])
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
-- DELETE
-- ----------------------------------------------------------------------------------------------------------------------
deleteDeleteSerieR :: Text -> UsuarioId -> SerieId -> Handler Value
deleteDeleteSerieR token userid seriid = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] []
    case maybeUser of 
        Just (Entity uid usuario) -> do
            [x] <- runDB $ selectKeysList [UserSerieSeriid ==. seriid, UserSerieUserid ==. userid] []
            runDB $ delete x
            sendStatusJSON noContent204 emptyObject
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------------------------------------------------
