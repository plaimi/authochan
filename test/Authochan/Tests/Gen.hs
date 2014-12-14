{- |
Module      :  $Header$
Description :  QuickCheck generators and mutators for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Tests.Gen where

import Control.Applicative
  (
  (<$>),
  (<*>),
  liftA,
  )
import Control.Monad.CryptoRandom
  (
  evalCRand,
  )
import Crypto.Hash
  (
  HMAC (HMAC),
  digestFromByteString,
  )
import Crypto.Random.DRBG
  (
  HashDRBG,
  ByteLength,
  genSeedLength,
  newGen,
  )
import Data.Byteable
  (
  toBytes,
  )
import qualified Data.ByteString as B
import Data.Maybe
  (
  fromJust,
  isJust,
  )
import Data.Tagged
  (
  Tagged,
  unTagged,
  )
import qualified Data.Text as T
import Database.Persist
  (
  Key,
  PersistEntity,
  PersistValue (PersistInt64),
  keyFromValues,
  )
import Test.QuickCheck
  (
  Gen,
  arbitrary,
  arbitraryBoundedIntegral,
  suchThat,
  vectorOf,
  )

import Authochan.Database
  (
  Client (Client),
  )
import Authochan.Key
  (
  newClientHandle,
  newClientSecret,
  newSessionID,
  )
import Authochan.Message
  (
  SignedMessage (MkSignedMessage),
  signMessage,
  )
import Test.MutableGen
  (
  mutateSized,
  )

genRandom :: Gen HashDRBG
genRandom = do
  let b = unTagged (genSeedLength :: Tagged HashDRBG ByteLength)
  Right e <- newGen <$> B.pack <$> vectorOf b arbitraryBoundedIntegral
  return e

genKey :: PersistEntity r => Gen (Key r)
genKey = do
  k <- arbitrary
  return $ either (error . T.unpack) id $ keyFromValues [PersistInt64 k]

genClient :: Gen Client
genClient = do
  g <- genRandom
  let Right (h, s) = evalCRand ((,) <$> newClientHandle <*> newClientSecret) g
  k <- genKey
  n <- arbitraryBoundedIntegral
  d <- T.pack <$> arbitrary
  return $ Client h s n k d

genMessage :: Client -> Gen SignedMessage
genMessage c = do
  g <- genRandom
  n <- arbitraryBoundedIntegral
  let Right s = evalCRand newSessionID g
  b <- B.pack <$> arbitrary
  return $ signMessage c s n b

mutateMessage :: SignedMessage -> Gen SignedMessage
mutateMessage (MkSignedMessage k sid n s b) = liftA fromJust g
  where g = suchThat gm isJust
        gm = do
          (k', sid', n', s', b') <- mutateSized (k, sid, n, toBytes s, b)
          return $ do
            s'' <- HMAC <$> digestFromByteString s'
            Just $ MkSignedMessage k' sid' n' s'' b'
