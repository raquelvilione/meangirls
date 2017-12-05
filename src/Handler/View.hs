{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.View where
-- ----------------------------------------------------------------------------------------------------------------------
-- IMPORT
-- ----------------------------------------------------------------------------------------------------------------------
import Control.Monad
import Import 
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Database.Persist.Postgresql 
-- ----------------------------------------------------------------------------------------------------------------------
-- GET
-- ----------------------------------------------------------------------------------------------------------------------
getSelecionadoAssistidoR :: Text -> UsuarioId -> EpisodioId -> Handler Value
getSelecionadoAssistidoR token userid epiid = do
    maybeUser <- runDB $ selectFirst [UsuarioToken ==. token] [] 
    case maybeUser of 
        Just (Entity uid usuario) -> do 
            viewEpisodiosEntity <- runDB $ selectList [ViewUserid ==. userid] []
            episodios <- mapM (\ entity -> runDB $  get404 $ viewEpid $ entityVal entity) viewEpisodiosEntity 
            usuario <- runDB $ get404 userid 
            sendStatusJSON ok200 (object ["resp" .= object [(usuarioNome usuario) .= toJSON episodios] ])
        _ -> do
            sendStatusJSON forbidden403 (object [ "resp" .= ("acao proibida"::Text)])
-- ----------------------------------------------------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------------------------------------------------