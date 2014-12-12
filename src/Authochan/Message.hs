{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Message verification for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Message where

import Control.Monad
  (
  guard,
  )
import Control.Monad.IO.Class
  (
  MonadIO,
  )
import Control.Monad.Reader
  (
  ReaderT,
  )
import Crypto.Hash
  (
  HMAC,
  SHA256,
  hmac,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Search
  (
  replace,
  )
import Data.Int
  (
  Int64,
  )
import Database.Persist
  (
  Entity (Entity),
  (=.),
  getBy,
  update,
  )
import Database.Persist.Sql
  (
  SqlBackend,
  )

import Authochan.Database

-- | A 'SignedMessage' contains a 'B.ByteString' message and signature data.
data SignedMessage = MkSignedMessage
  {smKey   :: B.ByteString -- ^ The client key/handle.
  ,smSID   :: B.ByteString -- ^ The session ID.
  ,smNonce :: Int64        -- ^ Nonce to protect against replay attacks.
  ,smSig   :: HMAC SHA256  -- ^ Message authentication code.
  ,smBody  :: B.ByteString -- ^ The signed data.
  } deriving Eq

serialiseList :: [B.ByteString] -> B.ByteString
-- | 'serialiseList' provides a safe way to serialise a list of
-- 'B.ByteString's
serialiseList = B.intercalate "\n" . map f
  where f = B8.cons '*' . BL.toStrict . replace "\n" (B8.pack "\n ")

serialiseMessage :: SignedMessage -> B.ByteString
-- | @'serialiseMessage' m@ is the data used when generating or verifying
-- @'smSig' m@.
serialiseMessage (MkSignedMessage h sid n _ b) =
  serialiseList [h, sid, B8.pack $ show n, b]

signMessage :: Client -> B.ByteString -> Int64 -> B.ByteString
            -> SignedMessage
-- | 'signMessage' generates a 'SignedMessage' from the provided 'Client',
-- nonce and 'B.ByteString' message.
signMessage c sid n b = MkSignedMessage h sid n sig b
  where m   = MkSignedMessage h sid n sig b
        sig = hmac sec $ serialiseMessage m
        h   = clientHandle c
        sec = clientSecret c

verifyMessage :: Client -> SignedMessage -> Maybe Int64
-- | 'verifyMessage' checks that a 'SignedMessage' has a valid signature from
-- the given 'Client' and that the message nonce is greater than the client's
-- previous nonce. Returns the new nonce if the signature is verified, or
-- 'Nothing' otherwise.
verifyMessage c (MkSignedMessage h sid n sig b) = do
  guard $ (h == clientHandle c) && (n > clientNonce c)
  guard $ sig == smSig (signMessage c sid n b)
  return n

authenticateMessage :: MonadIO m
                    => SignedMessage
                    -> ReaderT SqlBackend m (Maybe (Entity Client))
-- | 'authenticateMessage' checks that a 'SignedMessage' has a valid signature
-- from a client in the database using 'verifyMessage'. If the signature is
-- verified, the nonce in the database is updated and the 'Entity' of the
-- authenticated 'Client' is returned.
authenticateMessage sm = do
  maybeClient <- getBy $ UniqueClient $ smKey sm
  let v = do
        Entity cid c <- maybeClient
        n'           <- verifyMessage c sm
        return (cid, n')
  case v of
    Just (cid, n) -> update cid [ClientNonce =. n] >> return maybeClient
    _             -> return Nothing
