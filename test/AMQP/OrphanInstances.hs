{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : AMQP.OrphanInstances
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module AMQP.OrphanInstances (
  ) where

import AMQP.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.GenValidity.ByteString ()
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Protocol.AMQP.Elementary
import Protocol.AMQP.FieldValue
import Protocol.AMQP.Frame
import Protocol.AMQP.Translated
import Test.Validity.ParserOf (GenValid (..), suchThat)


deriving anyclass instance GenValid (InnerFrame Method)


deriving anyclass instance GenValid (InnerFrame ContentBody)


deriving anyclass instance GenValid (InnerFrame ContentHdr)


deriving anyclass instance GenValid (InnerFrame Heartbeat)


deriving anyclass instance GenValid Heartbeat


deriving anyclass instance GenValid ContentBody


deriving anyclass instance GenValid ContentHdr


deriving via Bool instance GenValid Bit


deriving anyclass instance GenValid FieldValue


deriving anyclass instance GenValid FieldTable


deriving via Word64 instance GenValid LongLongInt


deriving via Word32 instance GenValid LongInt


deriving via Word16 instance GenValid ShortInt


deriving via Word8 instance GenValid Octet


deriving via (Octet, LongInt) instance GenValid (DecimalValue)


deriving via (ByteString) instance GenValid LongString


deriving instance Generic ShortString


instance GenValid ShortString where
  genValid = ShortString <$> (genValid `suchThat` (\x -> BS.length x <= 255))


compileDerivingDecs
