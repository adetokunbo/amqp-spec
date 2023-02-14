{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.ParserOf (
  -- * functions
  roundtripped,
  roundtripSpecFor,
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


roundtripped :: (Eq a, ToBuilder a BB.Builder, ParserOf a) => a -> Bool
roundtripped x =
  let parser = A.parse parserOf bytes
      bytes = LBS.toStrict $ BB.toLazyByteString $ toBuilder x
   in Just x == A.maybeResult parser


roundtripSpecFor ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, ToBuilder a BB.Builder, ParserOf a) =>
  Spec
roundtripSpecFor = do
  let name = nameOf @a
  describe ("encoding and decoding a " ++ name) $ do
    it "should roundtrip all valid values successfully" $ do
      withMaxSuccess 1000 $ forAllValid @a roundtripped
