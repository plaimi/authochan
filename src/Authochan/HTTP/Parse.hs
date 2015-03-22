{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  $Header$
Description :  HTTP parsing for authochan.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Authochan.HTTP.Parse where

import Control.Applicative
  (
  (<*),
  (*>),
  (<$>),
  (<*>),
  (<|>),
  optional,
  )
import Data.Attoparsec.ByteString.Char8
  (
  Parser,
  (<?>),
  endOfInput,
  inClass,
  isHorizontalSpace,
  option,
  satisfy,
  sepBy,
  skipMany1,
  takeWhile1,
  )
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Maybe
  (
  catMaybes,
  )

spaced :: Parser a -> Parser a
-- | @'spaced' p@ strips away all space and horizontal tab characters
-- surrounding 'p'.
spaced p =
  AB.skipWhile isHorizontalSpace *> p <* AB.skipWhile isHorizontalSpace

parseToken :: Parser B.ByteString
-- | 'parseToken' parses a HTTP header token as specified in RFC 7230.
parseToken = takeWhile1 (inClass "-!#-'*+.0-9A-Z^-z|~") <?> "token"

parseQuotedString :: Parser B.ByteString
-- | 'parseQuotedString' parses a HTTP quoted-string as specified in RFC 7230.
parseQuotedString = "\"" *> go <?> "quoted-string"
  where go = (B.append <$> (takeWhile1 qdText <|> qPair) <*> go) <|>
             ("\"" *> return "")
        qPair = "\\" *> (B8.singleton <$> satisfy (inClass "\t !-~\x80-\xff"))
        qdText = inClass "\t !#-[]-~\x80-\xff"

parseCommaList :: Parser a -> Parser [a]
-- | @'parseCommaList' p@ parses a comma-separated list of 'p' according to
-- the #rule extension specified in RFC 7230.
parseCommaList p =
  catMaybes <$> optional p `sepBy` spaced "," <?> "comma-list"

parseParam :: Parser (B.ByteString, B.ByteString)
-- | 'parseParam' parses a single parameter similar to auth-param as specified
-- in RFC 7235.
parseParam = (,) <$> (parseToken <* spaced "=" <?> "lhs")
                 <*> (parseToken <|> parseQuotedString <?> "rhs")

parseCreds :: Parser (CI.CI B.ByteString, [(B.ByteString, B.ByteString)])
-- | 'parseCreds' parses a subset of HTTP credentials specified in RFC 7235.
parseCreds = (,) <$> (CI.mk <$> parseToken <?> "scheme")
                 <*> option [] (skipMany1 " " *> parseCommaList parseParam
                                             <?> "auth-params") <* endOfInput
                 <?> "credentials"
