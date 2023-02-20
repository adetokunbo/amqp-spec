{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.FrameSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.FrameSpec (
  spec,
) where

import AMQP.OrphanInstances ()
import Protocol.AMQP.Frame
import Protocol.AMQP.Translated (Method)
import Test.Hspec (Spec, describe)
import Test.Validity.ParserOf (roundtripSpecFor')


spec :: Spec
spec = describe "Frame types" $ do
  roundtripSpecFor' @(InnerFrame Heartbeat)
  roundtripSpecFor' @(InnerFrame ContentHdr)
  roundtripSpecFor' @(InnerFrame ContentBody)
  roundtripSpecFor' @(InnerFrame Method)
