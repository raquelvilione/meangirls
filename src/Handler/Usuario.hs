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
import Yesod.Auth.HashDB (setPassword,userPasswordHash, setPasswordHash)
-- ----------------------------------------------------------------------------------------------------------------------
-- POST
-- ----------------------------------------------------------------------------------------------------------------------
postUsuarioR :: Handler Value
postUsuarioR = do
    usu <- requireJsonBody :: Handler Usuario
    hashUser <- setPassword (usuarioEmail usu) usu
    usuarioId <- runDB $ insert hashUser
    sendStatusJSON created201 (object ["resp" .= (usuarioToken hashUser)])
-- ----------------------------------------------------------------------------------------------------------------------
-- 
-- ----------------------------------------------------------------------------------------------------------------------