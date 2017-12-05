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
--
-- ----------------------------------------------------------------------------------------------------------------------

    
