{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Protocol.AMQP.Attoparsec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Imports "Data.Attoparsec.Binary" and "Data.Attoparsec.ByteString", adds useful
support functions which are exported along with the imports from these base
modules
-}
module Protocol.AMQP.Attoparsec (
  -- * parse simple types
  anyInt8,
  anyInt16be,
  anyInt32be,
  anyInt64be,
  anyFloatbe,
  anyDoublebe,

  -- * prefixed parsing
  fixed,
  word16Pre,
  with1Prefix,
  with2Prefixes,

  -- * re-export the base modules
  module A,
) where

import Data.Attoparsec.Binary as A
import Data.Attoparsec.ByteString as A
import Data.Bits
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)


-- | Match a given @'Word16'@ prefix then apply the prefixed @Parser@.
with1Prefix :: Word16 -> A.Parser a -> A.Parser a
with1Prefix pre parser = A.word16be pre *> parser


-- | Match 2 given @'Word16'@ prefixes then the apply the corresponding @Parser@.
with2Prefixes :: Word16 -> [(Word16, A.Parser a)] -> A.Parser a
with2Prefixes pre prefixedParsers =
  let matchPres = A.choice . map (uncurry with1Prefix)
   in with1Prefix pre $ matchPres prefixedParsers


-- | Match a @'Word16'@ prefix and use it to create a @Parser@.
word16Pre :: (Word16 -> A.Parser a) -> A.Parser a
word16Pre f = A.anyWord16be >>= f


{- | Match a prefix that indicates the number of bytes that the following @Parser@
 must consume.
-}
fixed :: (Integral n) => n -> A.Parser a -> A.Parser a
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


-- | Match any big-endian float.
anyFloatbe :: Parser Float
anyFloatbe = castWord32ToFloat <$> anyWord32be


-- | Match any big-endian double.
anyDoublebe :: Parser Double
anyDoublebe = castWord64ToDouble <$> anyWord64be
