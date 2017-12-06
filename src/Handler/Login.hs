{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Login where
-- ----------------------------------------------------------------------------------------------------------------------
-- IMPORT
-- ----------------------------------------------------------------------------------------------------------------------
import Import
import Data.Aeson.Types
import Database.Persist.Postgresql
import Yesod.Auth.HashDB (setPassword)
-- ----------------------------------------------------------------------------------------------------------------------
-- POST
-- ----------------------------------------------------------------------------------------------------------------------
postLoginR :: Handler Value
postLoginR = do
    (email,senha) <- requireJsonBody :: Handler (Text,Text)
    maybeUsuario <- runDB $ getBy $ UsuarioLogin email senha
    case maybeUsuario of
        Just (Entity uid usuario) -> do
            newHashUser <- setPassword (usuarioEmail usuario) usuario
            runDB $ update uid [UsuarioToken =. (usuarioToken newHashUser)]
            sendStatusJSON ok200 (object ["resp" .= (usuarioToken newHashUser) ])
        _ -> 
            sendStatusJSON status404 (object ["resp" .= ("Usuário não cadastrado"::Text)] )
-- ----------------------------------------------------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------------------------------------------------