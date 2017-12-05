{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Usuario where
-- ----------------------------------------------------------------------------------------------------------------------
-- IMPORT
-- ----------------------------------------------------------------------------------------------------------------------
import Import
import Network.HTTP.Types.Status
import Data.Aeson.Types
import Database.Persist.Postgresql
-- ----------------------------------------------------------------------------------------------------------------------
-- POST
-- ----------------------------------------------------------------------------------------------------------------------
postUsuarioR :: Handler Value
postUsuarioR = do
    usu <- requireJsonBody :: Handler Usuario -- mandar vazio
    let hash = (usuarioNome usu ++ usuarioSenha usu)
    let usuario = Usuario (usuarioNome usu) (usuarioSobrenome usu) (usuarioEmail usu) (usuarioSenha usu) (hash)
    usuarioId <- runDB $ insert usuario
    sendStatusJSON created201 (object ["resp" .= hash ])
-- ----------------------------------------------------------------------------------------------------------------------
-- 
-- ----------------------------------------------------------------------------------------------------------------------

    