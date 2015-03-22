{- |
Module      :  $Header$
Description :  Test properties for authochan.
Copyright   :  (c) plaimi 2014-2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main (main) where

import Test.Framework
  (
  Test,
  defaultMain,
  )

import Authochan.Tests.Message
  (
  messageTests,
  )
import Authochan.Tests.HTTP
  (
  requestTests,
  )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = messageTests ++ requestTests
