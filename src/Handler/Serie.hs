{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Serie where

import Control.Monad
import Import 
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Database.Persist.Postgresql 


 -- https://docs.google.com/presentation/d/17GeGwmoYZnm2A86VomchGShw9PIA82Ld5FC-unXiySE/edit?usp=sharing

-- apikey 45167e2360d3bc4cac7f0e985b36bae5

postCadSerieR :: Handler Value 
postCadSerieR = do
    seri <- requireJsonBody :: Handler Serie
    seriid <- runDB $ insert seri
    sendStatusJSON created201 (object ["resp" .= fromSqlKey seriid])
    
deleteDeleteSerieR :: UsuarioId -> SerieId -> Handler Value
deleteDeleteSerieR userid seriid = do 
    [x] <- runDB $ selectKeysList [UserSerieSeriid ==. seriid, UserSerieUserid ==. userid] []
    runDB $ delete x
    sendStatusJSON noContent204 emptyObject

{-getListaSerieR :: Text -> Handler Value
getListaSerieR x = do
   --list <- runDB $ selectList [Filter SerieGeneros (Left $ mconcat ["%",x,"%"]) (BackendSpecificFilter "ILIKE")] []
   list <- runDB $ selectList [SerieGenre_ids ==. [x]] [] -- aqui é uma lista pois no banco é, ele só consegue comparar coisas iguais
   sendStatusJSON ok200 (object["resp" .= toJSON list])-}
    
    
