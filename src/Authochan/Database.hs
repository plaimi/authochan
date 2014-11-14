{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{- |
Module      :  $Header$
Description :  これはオートのデトベスです。
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Database where

import Data.ByteString
  (
  ByteString,
  )
import Data.Int
  (
  Int64,
  )
import Database.Persist.TH
  (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
  )
import Data.Text
  (
  Text,
  )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name           Text       -- user's ID on server, as text.
  deriving Show
Client
  handle         ByteString -- sent by the client to オートちゃん.
  secret         ByteString -- secret part of handle.
  nonce          Int64      -- last message nonce.
  user           UserId
  description    Text
  UniqueClient   handle
  deriving Show
|]
