{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.FieldValueSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.FieldValueSpec (
  spec,
) where

import AMQP.OrphanInstances ()
import Protocol.AMQP.FieldValue
import Test.Hspec (Spec, describe)
import Test.Validity.ParserOf (roundtripSpecFor')


spec :: Spec
spec = describe "FieldValue types" $ do
  roundtripSpecFor' @FieldValue
