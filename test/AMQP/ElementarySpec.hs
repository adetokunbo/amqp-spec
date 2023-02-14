{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.ElementarySpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.ElementarySpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.GenValidity.ByteString ()
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Protocol.AMQP.Elementary
import Test.Hspec (Spec, describe)
import Test.Validity.ParserOf (GenValid (..), roundtripSpecFor, suchThat)


spec :: Spec
spec = describe "Elementary types" $ do
  roundtripSpecFor @LongLongInt
  roundtripSpecFor @LongInt
  roundtripSpecFor @ShortInt
  roundtripSpecFor @Octet
  roundtripSpecFor @DecimalValue
  roundtripSpecFor @LongString
  roundtripSpecFor @ShortString


deriving via Word64 instance GenValid LongLongInt


deriving via Word32 instance GenValid LongInt


deriving via Word16 instance GenValid ShortInt


deriving via Word8 instance GenValid Octet


deriving via (Octet, LongInt) instance GenValid (DecimalValue)


deriving via (ByteString) instance GenValid LongString


deriving instance Generic ShortString


instance GenValid ShortString where
  genValid = ShortString <$> (genValid `suchThat` (\x -> BS.length x <= 255))
