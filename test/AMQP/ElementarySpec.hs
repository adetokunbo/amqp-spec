{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.ElementarySpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.ElementarySpec (spec) where

import AMQP.OrphanInstances ()
import Protocol.AMQP.Elementary
import Test.Hspec (Spec, describe)
import Test.Validity.ParserOf (roundtripSpecFor)


spec :: Spec
spec = describe "Elementary types" $ do
  roundtripSpecFor @LongLongInt
  roundtripSpecFor @LongInt
  roundtripSpecFor @ShortInt
  roundtripSpecFor @Octet
  roundtripSpecFor @DecimalValue
  roundtripSpecFor @LongString
  roundtripSpecFor @ShortString
