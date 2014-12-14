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
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Search
  (
  replace,
  split,
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
  counterexample,
  listOf,
  suchThat,
  )

import Authochan.Database
  (
  Client,
  clientNonce,
  )
import Authochan.Message
  (
  SignedMessage,
  serialiseList,
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
                           propDeserialiseList]]

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
