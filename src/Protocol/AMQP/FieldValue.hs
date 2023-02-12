{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.FieldValue (
  -- * data types
  FieldTable (..),
  FieldValue (..),

  -- * parserOf combinators
  bitAt,

  -- * toBuilder combinators
  onlyPrefixes,
  withPrefixes,
  buildBits,

  -- * re-exports
  module Data.Builder,
  module Protocol.AMQP.Elementary,
) where

import Data.Bits (setBit, testBit)
import Data.Builder (ToBuilder (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Char (chr)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Validity (Validity (..))
import Data.Validity.ByteString ()
import Data.Word (Word16)
import GHC.Generics (Generic)
import qualified Protocol.AMQP.Attoparsec as A
import Protocol.AMQP.Elementary


-- newtype Timestamp = Timestamp Word64
--   deriving stock (Eq, Ord, Show)
--   deriving (Num, Real, Integral, Bits, FiniteBits, Enum, Bounded, Read, ParserOf) via Word64

-- instance ToBuilder Timestamp BB.Builder where
--   toBuilder (Timestamp x) = word64BE x

newtype FieldTable = FieldTable [(ShortString, FieldValue)]
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ToBuilder FieldTable BB.Builder where
  toBuilder (FieldTable []) = word32BE 0
  toBuilder (FieldTable xs) =
    let subs = toLazyByteString $ mconcat (map (\(x, y) -> toBuilder x <> toBuilder y) xs)
        size = LBS.length subs
     in word32BE (fromIntegral size) <> lazyByteString subs


instance ParserOf FieldTable where
  parserOf =
    let parsePair = (,) <$> parserOf <*> parserOf
        parsePairs = A.many' parsePair
     in fmap FieldTable $ A.anyWord32be >>= flip A.fixed parsePairs


data FieldValue
  = FBool !Bit
  | FInt8 !Int8
  | FInt16 !Int16
  | FInt32 !Int32
  | FInt64 !Int64
  | FFloat !Float
  | FDouble !Double
  | FVoid
  | FDecimal !DecimalValue
  | FString !LongString
  | FTimestamp !LongLongInt
  | FByteArray !ByteString
  | FFieldArray ![FieldValue]
  | FFieldTable !FieldTable
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ToBuilder FieldValue BB.Builder where
  toBuilder (FBool x) = char7 't' <> toBuilder x
  toBuilder (FInt8 x) = char7 'b' <> int8 x
  toBuilder (FInt16 x) = char7 's' <> int16BE x
  toBuilder (FInt32 x) = char7 'I' <> int32BE x
  toBuilder (FInt64 x) = char7 'l' <> int64BE x
  toBuilder (FFloat x) = char7 'f' <> floatBE x
  toBuilder (FDouble x) = char7 'd' <> doubleBE x
  toBuilder (FDecimal x) = char7 'D' <> toBuilder x
  toBuilder (FString x) = char7 'S' <> (toBuilder x)
  toBuilder (FFieldArray []) = char7 'A' <> word32BE 0
  toBuilder (FFieldArray xs) =
    let subs = toLazyByteString $ mconcat (map toBuilder xs)
        size = LBS.length subs
     in char7 'A' <> word32BE (fromIntegral size) <> lazyByteString subs
  toBuilder (FTimestamp x) = char7 'T' <> toBuilder x
  toBuilder (FFieldTable x) = char7 'F' <> toBuilder x
  toBuilder (FVoid) = char7 'V'
  toBuilder (FByteArray b) = char7 'x' <> (word32BE $ fromIntegral $ BS.length b) <> byteString b


instance ParserOf FieldValue where
  parserOf =
    ((chr . fromIntegral) <$> A.anyWord8) >>= \case
      't' -> FBool <$> parserOf
      'b' -> FInt8 <$> A.anyInt8
      's' -> FInt16 <$> A.anyInt16be
      'I' -> FInt32 <$> A.anyInt32be
      'l' -> FInt64 <$> A.anyInt64be
      'f' -> FFloat <$> A.anyFloatbe
      'd' -> FDouble <$> A.anyDoublebe
      'D' -> FDecimal <$> parserOf
      'S' -> FString <$> parserOf
      'T' -> FTimestamp <$> parserOf
      'F' -> FFieldTable <$> parserOf
      'A' -> fmap FFieldArray $ A.anyWord32be >>= flip A.fixed (A.many' parserOf)
      'V' -> pure FVoid
      'x' -> A.anyWord32be >>= fmap FByteArray . A.take . fromIntegral
      x -> fail $ "unknown field type: " ++ show x


-- data TrialConfirm -- prefix 85
--   = SelectOk -- prefix 10
--   | Select {selectNoWait :: !Bit} -- prefix 11
--   deriving (Eq, Show)

-- instance ToBuilder TrialConfirm BB.Builder where
--   toBuilder SelectOk = onlyPrefixes 85 10
--   toBuilder (Select x) = withPrefixes 85 11 x

-- instance ParserOf TrialConfirm where
--   parserOf =
--     matchTwoPrefixes
--       85
--       [ (10, pure SelectOk)
--       , (11, Select <$> parserOf)
--       ]

-- data DaNackData = DaNackData
--   { dnDeliveryTag :: !LongLongInt
--   , dnMultiple :: !Bit
--   , dnRequeue :: !Bit
--   }
--   deriving (Eq, Show)

-- instance ToBuilder DaNackData BB.Builder where
--   toBuilder x =
--     mconcat
--       [ toBuilder (dnDeliveryTag x)
--       , buildBits [dnMultiple x, dnRequeue x]
--       ]

-- instance ParserOf DaNackData where
--   parserOf = do
--     dnDeliveryTag <- parserOf
--     packed <- parserOf
--     let dnMultiple = bitAt packed 0
--         dnRequeue = bitAt packed 1
--     pure $ DaNackData {dnMultiple, dnRequeue, dnDeliveryTag}

bitAt :: Word16 -> Int -> Bit
bitAt x pos = coerce $ testBit x pos


buildBits :: [Bit] -> Builder
buildBits bits =
  let withPos = zip [0 ..] $ map coerce bits
      step (pos, check) acc = if check then acc `setBit` pos else acc
   in word16BE $ foldr step 0 withPos


onlyPrefixes :: Word16 -> Word16 -> BB.Builder
onlyPrefixes a b = word16BE a <> word16BE b


withPrefixes :: ToBuilder a BB.Builder => Word16 -> Word16 -> a -> BB.Builder
withPrefixes a b builder = onlyPrefixes a b <> toBuilder builder
