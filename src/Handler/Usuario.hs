{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Usuario where

import Import
-- import Network.HTTP.Types.Status
import Data.Aeson.Types
import Database.Persist.Postgresql

postUsuarioR :: Handler Value
postUsuarioR = do
    usu <- requireJsonBody :: Handler Usuario
    usuid <- runDB $ insert usu
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey usuid)])


    