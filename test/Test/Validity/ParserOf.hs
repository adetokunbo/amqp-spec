{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.ParserOf (
  -- * functions
  roundtripped,
  roundtripped',
  roundtripSpecFor,
  roundtripSpecFor',
  module Test.Validity,
  module Test.QuickCheck,
) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)
import qualified Protocol.AMQP.Attoparsec as A
import Protocol.AMQP.Elementary (ParserOf (..), ToBuilder (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (suchThat, withMaxSuccess)
import Test.Validity (GenValid (..), forAllValid)
import Test.Validity.Utils (nameOf)


-- Like 'roundtripped', but builds the bytes again and compares that. This gets
-- round issues with comparing data structures that contain floats and doubles
-- that might be NaN and fail the equality test
roundtripped' :: (Eq a, ToBuilder a BB.Builder, ParserOf a) => a -> Bool
roundtripped' x =
  let parser = A.parse parserOf bytes
      asBytes = LBS.toStrict . BB.toLazyByteString . toBuilder
      bytes = asBytes x
   in Just bytes == (fmap asBytes $ A.maybeResult parser)


-- Encodes and decodes a value and ensures they are equal
roundtripped :: (Eq a, ToBuilder a BB.Builder, ParserOf a) => a -> Bool
roundtripped x =
  let parser = A.parse parserOf bytes
      bytes = LBS.toStrict $ BB.toLazyByteString $ toBuilder x
   in Just x == A.maybeResult parser


-- A spec the uses 'roundtripped' in a property test
roundtripSpecFor ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, ToBuilder a BB.Builder, ParserOf a) =>
  Spec
roundtripSpecFor = do
  let name = nameOf @a
  describe ("encoding and decoding a " ++ name) $ do
    it "should roundtrip all valid values successfully" $ do
      withMaxSuccess 1000 $ forAllValid @a roundtripped


-- A spec the uses 'roundtripped'' in a property test
roundtripSpecFor' ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, ToBuilder a BB.Builder, ParserOf a) =>
  Spec
roundtripSpecFor' = do
  let name = nameOf @a
  describe ("encoding and decoding a " ++ name) $ do
    it "should roundtrip all valid values successfully" $ do
      withMaxSuccess 1000 $ forAllValid @a roundtripped'
