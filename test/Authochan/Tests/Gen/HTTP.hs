{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  HTTP generators and mutators for authochan.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Tests.Gen.HTTP where

import Blaze.ByteString.Builder
  (
  toByteString,
  )
import Control.Applicative
  (
  (<$>),
  (<*>),
  )
import Control.Monad.CryptoRandom
  (
  evalCRand,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.Char
  (
  isAlphaNum,
  )
import Data.Int
  (
  Int64,
  )
import Data.Monoid
  (
  (<>),
  )
import qualified Data.Text as T
import Network.HTTP.Types
  (
  Method,
  Header,
  HeaderName,
  encodePath,
  renderStdMethod,
  toQuery,
  )
import Network.Wai
  (
  defaultRequest,
  rawPathInfo,
  rawQueryString,
  requestMethod,
  requestHeaders,
  )
import Network.Wai.Test
  (
  SRequest (SRequest),
  setPath,
  )
import Test.QuickCheck
  (
  Gen,
  arbitrary,
  elements,
  listOf,
  listOf1,
  oneof,
  suchThat,
  vectorOf,
  )

import Authochan.Key
  (
  newClientHandle,
  newSessionID,
  )
import Authochan.Tests.Gen
  (
  genRandom,
  )
import Test.MutableGen
  (
  mutate,
  )

genHeaderName :: Gen HeaderName
genHeaderName = oneof [elements ["Content-Type", "Host"]
                      ,(CI.mk . B.pack) <$> arbitrary]

genHeader :: [HeaderName] -> Gen Header
genHeader hns = elements hns >>= \e -> ((,) e . B.pack) <$> arbitrary

genMethod :: Gen Method
genMethod = oneof $ (B8.pack <$> listOf1 (suchThat arbitrary isAlphaNum)) :
                    map (return . renderStdMethod) (enumFrom minBound)

genPath :: Gen B.ByteString
genPath = do
  p <- map T.pack <$> arbitrary
  q <- toQuery <$> (arbitrary :: Gen [(String, String)])
  return $ toByteString $ encodePath p q

genSRequest :: Gen SRequest
genSRequest = do
  m <- genMethod
  p <- genPath
  n <- listOf1 genHeaderName
  h <- listOf $ genHeader n
  b <- LB.pack <$> arbitrary
  let r = setPath (defaultRequest {requestMethod = m, requestHeaders = h}) p
  return $ SRequest r b

genSignedSRequest :: Gen SRequest
genSignedSRequest = do
  SRequest r b <- genSRequest
  g <- genRandom
  let Right (k, s) = evalCRand ((,) <$> newClientHandle <*> newSessionID) g
  n <- arbitrary :: Gen Int64
  h <- vectorOf 64 $ elements $ ['0'..'9'] ++ ['a'..'f']
  let ah = ("Authorization", "Authochan key="       <> k
                                  <> ", sid="       <> s
                                  <> ", nonce="     <> B8.pack (show n)
                                  <> ", signature=" <> B8.pack h)
  return $  SRequest (r {requestHeaders = ah : requestHeaders r}) b

mutateSRequest :: SRequest -> Gen SRequest
mutateSRequest (SRequest r b) = do
  (m, p, h, b') <- mutate (requestMethod r
                          ,rawPathInfo r <> rawQueryString r
                          ,requestHeaders r, b)
  return $ SRequest (setPath (r {requestMethod = m, requestHeaders = h}) p) b'
