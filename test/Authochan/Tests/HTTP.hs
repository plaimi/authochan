{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  HTTP request verification tests for authochan.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Tests.HTTP (requestTests) where

import Control.Monad
  (
  forM,
  guard,
  msum,
  )
import Crypto.Hash
  (
  hashlazy,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Char
  (
  isHexDigit,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.Int
  (
  Int64,
  )
import Data.List
  (
  nub,
  )
import Data.Maybe
  (
  isJust,
  )
import Network.HTTP.Types
  (
  Header,
  )
import Network.Wai
  (
  rawPathInfo,
  rawQueryString,
  requestHeaders,
  requestMethod,
  )
import Network.Wai.Test
  (
  SRequest (SRequest),
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
  counterexample,
  elements,
  listOf,
  listOf1,
  )

import Authochan.HTTP
  (
  filterHeaders,
  parseAuthHeader,
  requestToMessage,
  )
import Authochan.Tests.Gen.HTTP
  (
  genHeader,
  genHeaderName,
  genSignedSRequest,
  mutateSRequest,
  )

requestTests :: [Test]
requestTests =
  [testGroup "HTTP request verification"
             [testProperty "headers are filtered and sorted correctly"
                           propFilterHeaders
             ,testProperty "well-formed signed requests are accepted"
                           propRequestWellFormed
             ,testProperty "signature protects specific parts of request"
                           propStripRequest]]

stripRequest :: SRequest -> Maybe ([B.ByteString], Int64, [Header])
stripRequest (SRequest r b) = do
  p             <- parseAuthHeader =<< lookup "Authorization" rh
  [k, s, n, sx] <- forM ["key", "sid", "nonce", "signature"] (flip lookup p)
  n'            <- msum [Just x | (x, "") <- reads $ B8.unpack n]
  guard $ B.length sx == 64 && B8.all isHexDigit sx
  return $ ([k, s, sx, LB.toStrict b, requestMethod r, rawPathInfo r
            ,rawQueryString r], n', filterHeaders ["Content-Type", "Host"] rh)
  where rh = requestHeaders r

propFilterHeaders :: Gen Property
propFilterHeaders = do
  f  <- nub <$> listOf1 genHeaderName
  nx <- listOf1 genHeaderName
  hs <- listOf $ elements [f, nx] >>= genHeader
  let r  = filterHeaders f hs
  return $ r === concatMap (\x -> filter ((== x) . fst) r) f

propRequestWellFormed :: Gen Property
propRequestWellFormed = do
  SRequest r b <- genSignedSRequest
  return $ counterexample (show r) $
           isJust $ requestToMessage r (hashlazy b)

propStripRequest :: Gen Property
propStripRequest = do
  r  <- genSignedSRequest
  r' <- mutateSRequest r
  return $ counterexample (s r ++ "\n" ++ s r')
         $ (stripRequest r == stripRequest r') === (m r == m r')
  where m (SRequest r b) = requestToMessage r  (hashlazy b)
        s sr@(SRequest r b) = show (r, b, stripRequest sr)
