{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Test properties for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main (main) where

import Control.Applicative
  (
  (<$>),
  (<*>),
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
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Search
  (
  replace,
  split,
  )
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
  PersistValue(PersistInt64),
  keyFromValues,
  )
import Test.Framework
  (
  Test,
  defaultMain,
  testGroup,
  )
import Test.Framework.Providers.QuickCheck2
  (
  testProperty,
  )
import Test.QuickCheck
  (
  Gen,
  Property,
  (===),
  (==>),
  arbitrary,
  arbitraryBoundedIntegral,
  counterexample,
  listOf,
  suchThat,
  vectorOf,
  )

import Authochan.Database
  (
  Client (Client),
  clientNonce,
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
  serialiseList,
  signMessage,
  smNonce,
  verifyMessage,
  )
import Test.MutableGen
  (
  mutateSized,
  )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Message verification"
                   [testProperty "checks validity of nonce"
                                 propValidatedNonce
                   ,testProperty "checks integrity of the whole message"
                                 propIntegrity
                   ,testProperty "reversibly serialises lists"
                                 propDeserialiseList]]

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
mutateMessage (MkSignedMessage k sid n s b) = g >>= return . fromJust
  where g = suchThat gm isJust
        gm = do
          (k', sid', n', s', b') <- mutateSized (k, sid, n, toBytes s, b)
          return $ do
            s'' <- HMAC <$> digestFromByteString s'
            Just $ MkSignedMessage k' sid' n' s'' b'

validatedNonce :: Client -> SignedMessage -> Property
validatedNonce c m = verifyMessage c m === ex
  where ex | n' > clientNonce c = Just n'
           | otherwise          = Nothing
        n' = smNonce m

propValidatedNonce :: Gen Property
propValidatedNonce = do
  c <- genClient
  validatedNonce c <$> genMessage c

propIntegrity :: Gen Property
propIntegrity = do
  c <- genClient
  m <- suchThat (genMessage c) $ \x -> smNonce x > clientNonce c
  m' <- mutateMessage m
  return $ m /= m' ==> verifyMessage c m' === Nothing

propDeserialiseList :: Gen Property
propDeserialiseList = do
  l <-listOf (B.pack <$> listOf arbitrary)
  let s = serialiseList l
      d = drop 1 $ map (LB.toStrict . replace "\n " (B8.pack "\n")) $
                   split "\n*" $ B8.cons '\n' s
  return $ counterexample ("s = " ++ show s) $ l === d
