{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.Attoparsec (
  anyInt8,
  anyInt16be,
  anyInt32be,
  anyInt64be,
  anyFloatbe,
  anyDoublebe,
  fixed,
  word16Pre,
  with1Prefix,
  with2Prefixes,
  module A,
) where

import Data.Attoparsec.Binary as A
import Data.Attoparsec.ByteString as A
import Data.Bits
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.ReinterpretCast (wordToDouble, wordToFloat)
import Data.Word (Word16)


word16Pre :: (Word16 -> A.Parser a) -> A.Parser a
word16Pre f = A.anyWord16be >>= f


fixed :: Integral n => n -> A.Parser a -> A.Parser a
fixed i p = do
  intermediate <- A.take $ fromIntegral i
  case A.parseOnly (p <* A.endOfInput) intermediate of
    Left x -> fail x
    Right x -> pure x


byteSize :: (FiniteBits a) => a -> Int
byteSize = (`div` 8) . finiteBitSize


pack :: (FiniteBits a, Num a) => BS.ByteString -> a
pack = BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0


anyIntN' :: forall a. (FiniteBits a, Num a) => (BS.ByteString -> a) -> Parser a
anyIntN' = flip fmap $ A.take $ byteSize (0 :: a)


-- | Match any int8.
anyInt8 :: Parser Int8
anyInt8 = anyIntN' pack


-- | Match any 16-bit big-endian int.
anyInt16be :: Parser Int16
anyInt16be = anyIntN' pack


-- | Match any 32-bit big-endian int.
anyInt32be :: Parser Int32
anyInt32be = anyIntN' pack


-- | Match any 64-bit big-endian int.
anyInt64be :: Parser Int64
anyInt64be = anyIntN' pack


-- | Many any big-endian float.
anyFloatbe :: Parser Float
anyFloatbe = wordToFloat <$> anyWord32be


-- | Many any big-endian float.
anyDoublebe :: Parser Double
anyDoublebe = wordToDouble <$> anyWord64be


with1Prefix :: Word16 -> A.Parser a -> A.Parser a
with1Prefix pre parser = A.word16be pre *> parser


with2Prefixes :: Word16 -> [(Word16, A.Parser a)] -> A.Parser a
with2Prefixes pre prefixedParsers =
  let matchPres xs = A.choice $ map (\(nextPre, p) -> with1Prefix nextPre p) xs
   in with1Prefix pre $ matchPres prefixedParsers
