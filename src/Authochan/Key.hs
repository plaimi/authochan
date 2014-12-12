{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  Random key generation for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.Key where

import Control.Applicative
  (
  (<$>),
  )
import Control.Monad.CryptoRandom
  (
  MonadCRandom,
  getBytes,
  )
import Data.Bits
  (
  (.&.),
  shiftR,
  )
import qualified Data.ByteString as B

generateKey :: (MonadCRandom e f, Functor f) => Integer -> f B.ByteString
-- | 'generateKey' generates a cryptographically secure random 'B.ByteString'
-- at the given security level (in bits). The resulting string consists of
-- base-58 characters, making it is easy to handle by humans.
generateKey b = B.map (B.index c . fromIntegral) . B.concat <$> go [] l
  where go x n | n > 0     = do
          bs <- (B.filter (< r) . B.map (.&. m)) <$> getBytes n
          go (bs : x) (n - B.length bs)
               | otherwise = return x
        m = last $ takeWhile (r - 1 <=) $ iterate (`shiftR` 1) 255
        l = ceiling $ fromInteger b / logBase 2 (fromIntegral r :: Double)
        r = fromIntegral $ B.length c
        c = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

newClientHandle :: (MonadCRandom e f, Functor f) => f B.ByteString
-- | 'newClientHandle' generates a random client handle.
newClientHandle = generateKey 64

newSessionID :: (MonadCRandom e f, Functor f) => f B.ByteString
-- | 'newSessionHandle' generates a new session ID.
newSessionID = generateKey 64

newClientSecret :: (MonadCRandom e f, Functor f) => f B.ByteString
-- | 'newClientSecret' generates a new secure client secret.
newClientSecret = generateKey 256
