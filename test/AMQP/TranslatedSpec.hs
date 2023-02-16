{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.TranslatedSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.TranslatedSpec (
  spec,
) where

import AMQP.OrphanInstances ()
import AMQP.TH (
  compileRoundTripSpecDecs,
 )
import Protocol.AMQP.Translated
import Test.Hspec (Spec, describe)


spec :: Spec
spec = describe "Translated data types" $ $compileRoundTripSpecDecs
