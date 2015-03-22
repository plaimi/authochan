{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  HTTP request verification for authochan.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.HTTP where

import Control.Monad
  (
  guard,
  msum,
  )
import Crypto.Hash
  (
  Digest,
  HMAC (HMAC),
  SHA256,
  digestFromByteString,
  digestToHexByteString,
  )
import Data.Attoparsec.ByteString
  (
  feed,
  maybeResult,
  parse,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as X
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Functor
  (
  (<$>),
  )
import Data.Monoid
  (
  (<>),
  )
import Network.HTTP.Types
  (
  Header,
  HeaderName,
  )
import Network.Wai
  (
  Request,
  rawPathInfo,
  rawQueryString,
  requestHeaders,
  requestMethod,
  )

import Authochan.Message
  (
  SignedMessage (MkSignedMessage),
  serialiseList,
  )
import Authochan.HTTP.Parse
  (
  parseCreds,
  )

serialiseHeaders :: [Header] -> B.ByteString
-- | @'serialiseHeaders' hs@ returns 'B.ByteString' where each header in 'hs'
-- is unfolded and written on the form "h: v". Headers are separated by
-- newline characters.
serialiseHeaders = B8.intercalate "\n" . map bs
  where bs (a, b) = B.concat [CI.original a, ": ", unfoldHeader b]

unfoldHeader :: B.ByteString -> B.ByteString
-- | 'unfoldHeader' replaces a multiline-folded header value with
-- a single-line value, as described in RFC 7230 section 3.2.4.
unfoldHeader =
  B8.intercalate " " . map (B8.dropWhile (`elem` "\t ")) . B8.split '\n'

filterHeaders :: [HeaderName] -> [Header] -> [Header]
-- | @'filterHeaders' hns hs@ returns the 'Header's in 'hs' whose names match
-- the 'HeaderName's in 'hns', following the order and letter case of 'hns'.
filterHeaders hns hs = [(hn, snd h) | hn <- hns, h <- hs, fst h == hn]

parseAuthHeader :: B.ByteString -> Maybe [(B.ByteString, B.ByteString)]
-- | 'parseAuthHeader' parses the parametre list of an
-- "Authorization: Authochan ..." header.
parseAuthHeader h = do
  (sc, ps) <- maybeResult . flip feed "" $ parse parseCreds h
  guard $ sc == "Authochan"
  return ps

requestTarget :: Request -> B.ByteString
-- | @'requestTarget' r@ returns the raw path and raw query of 'r'. An empty
-- path is represented by a single slash. The query string is prepended by
-- a question mark unless it is empty.
requestTarget r = cpath (rawPathInfo r) <> cquery (rawQueryString r)
  where cpath ""   = "/"
        cpath p    = p
        cquery "?" = ""
        cquery q   = q

requestToMessage :: Request -> Digest SHA256 -> Maybe SignedMessage
-- | 'requestToMessage' takes a 'Request' and the 'SHA256' 'Digest' of the
-- request body, and constructs a 'SignedMessage' if the request has
-- well-formed signature data.
requestToMessage r d = do
  p       <- parseAuthHeader =<< lookup "Authorization" (requestHeaders r)
  key     <- lookup "key" p
  sid     <- lookup "sid" p
  ns      <- B8.unpack <$> lookup "nonce" p
  nonce   <- msum [Just x | (x, "") <- reads ns]
  (sb, z) <- X.decode <$> lookup "signature" p
  guard $ B.null z
  sig     <- digestFromByteString sb
  return $ MkSignedMessage key sid nonce (HMAC sig)
                         $ serialiseList ["HTTP " <> requestMethod r
                                         ,requestTarget r
                                         ,serialiseHeaders .
                                          filterHeaders ["Content-Type"
                                                        ,"Host"]
                                                      $ requestHeaders r
                                         ,digestToHexByteString d]
