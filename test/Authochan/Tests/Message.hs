{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Message verification tests for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Tests.Message (messageTests) where

import Control.Applicative
  (
  (<$>),
  )
import Control.Monad
  (
  mfilter,
  )
import Crypto.Hash
  (
  Digest,
  SHA256,
  hmac,
  hmacGetDigest,
  digestToHexByteString,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Search
  (
  replace,
  split,
  )
import Data.Int
  (
  Int64,
  )
import Test.Framework
  (
  Test,
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
  conjoin,
  counterexample,
  listOf,
  once,
  suchThat,
  )

import Authochan.Database
  (
  Client,
  clientNonce,
  )
import Authochan.Message
  (
  SignedMessage (MkSignedMessage),
  serialiseList,
  serialiseMessage,
  smNonce,
  verifyMessage,
  )
import Authochan.Tests.Gen
  (
  genClient,
  genMessage,
  mutateMessage,
  )

messageTests :: [Test]
messageTests =
  [testGroup "Message verification"
             [testProperty "checks validity of nonce"
                           propValidatedNonce
             ,testProperty "checks integrity of the whole message"
                           propIntegrity
             ,testProperty "reversibly serialises lists"
                           propDeserialiseList
             ,testProperty "serialises and hashes sample messages correctly"
                           propSamples]]

samples :: [(B.ByteString, B.ByteString, Int64, B.ByteString, B.ByteString
            ,B.ByteString)]
samples =
  [("", "", 0, "", ""
   ,"4233049299d5a936502fbbfff357c2bbc90d2eb19758da65071dcaa27d3f699f")
  ,("key", "sid", 1, "body", "secret"
   ,"20eacc98d0da979b4d935e96bf7af1b6b4737c230651a340731763042771fed3")
  ,("abc", "def\n123", 456, "789", "x"
   ,"2d918ad69dfafce35e31ae837770a8b2973b9bea84a5fe4cadc3daef14fbecf3")
  ,("abc", "def", 123, "456\n789", "x"
   ,"3aa4da3b436836ba7fbe7fea0f40b32ad935815e341f5ef3b3d3fb9a868138e6")
  ,("zZVQN4t1xhi", "nMLkXgKAOLC", 2233779194633876839, "Example.\n"
   ,"PSLdvMMhdocwYSsjet8FJbpjrZUWufZufptssZNAfuFu"
   ,"29c4929e112cbbe6090e2844e07ac00addcd535a3a1619a546c886fb04e632e7")]

validatedNonce :: Client -> SignedMessage -> Property
validatedNonce c m =
  verifyMessage c m === mfilter (> clientNonce c) (Just $ smNonce m)

propValidatedNonce :: Gen Property
propValidatedNonce = do
  c <- genClient
  validatedNonce c <$> genMessage c

propIntegrity :: Gen Property
propIntegrity = do
  c  <- genClient
  m  <- suchThat (genMessage c) $ \x -> smNonce x > clientNonce c
  m' <- mutateMessage m
  return $ m /= m' ==> verifyMessage c m' === Nothing

propDeserialiseList :: Gen Property
propDeserialiseList = do
  l <-listOf (B.pack <$> listOf arbitrary)
  let s = serialiseList l
      d = drop 1 $ map (LB.toStrict . replace "\n " (B8.pack "\n")) $
                   split "\n*" $ B8.cons '\n' s
  return $ counterexample ("s = " ++ show s) $ l === d

propSamples :: Property
propSamples = once . conjoin $ do
  (k, sid, n, b, sec, sig) <- samples
  let ser = serialiseMessage $ MkSignedMessage k sid n undefined b
      md = hmacGetDigest (hmac sec ser) :: Digest SHA256
      ce = concat $ zipWith (++)
             ["key", show k
             ,"sid", show sid
             ,"nonce", show n
             ,"body", show b
             ,"serialised", show $ B8.unpack ser
             ,"secret", show sec
             ,"hmac", show sig]
             (cycle [" = ", "\n"])
  return $ counterexample ce $ digestToHexByteString md === sig
