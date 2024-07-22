{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Protocol.AMQP.Elementary
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides types that correspond to the elementary types recognized by the AMQP
specification.

These are mostly @newtypes@ of simple haskell types with 'ParserOf' and
'ToBuilder' instances that implement the encoding and decoding required by
the specification.
-}
module Protocol.AMQP.Elementary (
  -- * elementary data types
  DecimalValue (..),
  ShortString (..),
  mkShortString,
  unsafeMkShortString,
  LongString (..),
  Bit (..),
  Octet (..),
  LongInt (..),
  LongLongInt (..),
  ShortInt (..),

  -- * ParserOf
  ParserOf (..),

  -- * re-exports
  module Data.Builder,
) where

import Data.Bits (Bits, FiniteBits)
import Data.Builder (ToBuilder (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as BB
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Validity (Validity (..), check)
import Data.Validity.ByteString ()
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Protocol.AMQP.Attoparsec as A


newtype LongLongInt = LongLongInt Word64
  deriving stock (Eq, Ord, Show)
  deriving (Num, Real, Integral, Bits, FiniteBits, Enum, Bounded, Read, ParserOf, Validity) via Word64


instance ParserOf Word64 where
  parserOf = A.anyWord64be


instance ToBuilder LongLongInt BB.Builder where
  toBuilder (LongLongInt x) = word64BE x


newtype LongInt = LongInt Word32
  deriving stock (Eq, Ord, Show)
  deriving (Num, Real, Integral, Bits, FiniteBits, Enum, Bounded, Read, ParserOf, Validity) via Word32


instance ParserOf Word32 where
  parserOf = A.anyWord32be


instance ToBuilder LongInt BB.Builder where
  toBuilder (LongInt x) = word32BE x


newtype ShortInt = ShortInt Word16
  deriving stock (Eq, Ord, Show)
  deriving (Num, Real, Integral, Bits, FiniteBits, Enum, Bounded, Read, ParserOf, Validity) via Word16


instance ParserOf Word16 where
  parserOf = A.anyWord16be


instance ToBuilder ShortInt BB.Builder where
  toBuilder (ShortInt x) = word16BE x


newtype Bit = Bit Bool
  deriving stock (Eq, Ord, Show)
  deriving (Bits, FiniteBits, Enum, Bounded, Read, ParserOf, Validity) via Bool


instance ParserOf Bool where
  parserOf = (/= 0) <$> A.anyWord8


instance ToBuilder Bit BB.Builder where
  toBuilder (Bit x) = word8 $ fromIntegral $ fromEnum x


newtype Octet = Octet Word8
  deriving stock (Eq, Ord, Show)
  deriving (Num, Bits, FiniteBits, Enum, Bounded, Real, Read, Integral, ParserOf, Validity) via Word8


instance ToBuilder Octet BB.Builder where
  toBuilder (Octet x) = word8 x


newtype DecimalValue = DecimalValue (Octet, LongInt)
  deriving (Eq, Ord, Show)
  deriving (Validity) via (Octet, LongInt)


class ParserOf a where
  parserOf :: A.Parser a


instance ParserOf Word8 where
  parserOf = A.anyWord8


instance ToBuilder DecimalValue BB.Builder where
  toBuilder (DecimalValue (a, b)) = toBuilder a <> toBuilder b


instance ParserOf DecimalValue where
  parserOf = fmap DecimalValue $ (,) <$> parserOf <*> parserOf


newtype LongString = LongString ByteString
  deriving (Eq, Ord, Show)
  deriving (Validity) via ByteString


instance ToBuilder LongString BB.Builder where
  toBuilder (LongString b) = word32BE (fromIntegral $ BS.length b) <> byteString b


instance ParserOf LongString where
  parserOf = A.anyWord32be >>= fmap LongString . A.take . fromIntegral


newtype ShortString = ShortString ByteString
  deriving (Eq, Ord, Show)


instance Validity ShortString where
  validate (ShortString n) = check (BS.length n <= 255) "The 'ShortString' length is <= 255"


unsafeMkShortString :: Text -> ShortString
unsafeMkShortString = ShortString . Text.encodeUtf8 
  
mkShortString :: Text -> Either Text ShortString
mkShortString x =
  let encoded = Text.encodeUtf8 x
      tooLarge = BS.length encoded > 255
   in if tooLarge then Left "too large!" else Right $ ShortString encoded


instance ToBuilder ShortString BB.Builder where
  toBuilder (ShortString b) = word8 (fromIntegral $ BS.length b) <> byteString b


instance ParserOf ShortString where
  parserOf = A.anyWord8 >>= fmap ShortString . A.take . fromIntegral
