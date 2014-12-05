{- |
Module      :  $Header$
Description :  Test properties for authochan.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Main (main) where

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
  Property,
  (===),
  )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Example Group"
                   [testProperty "example property" propExample]]

propExample :: Property
propExample = reverse "zyx" === "xyz"
