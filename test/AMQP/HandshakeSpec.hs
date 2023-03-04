{-# LANGUAGE OverloadedStrings #-}
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
module AMQP.HandshakeSpec (
  spec,
) where

import AMQP.OrphanInstances ()
import Data.Attoparsec.Framer.Testing (linkedSrcAndSink)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Protocol.AMQP.Elementary
import Protocol.AMQP.Frame
import Protocol.AMQP.Handshake
import Protocol.AMQP.Translated
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Gen, Property)
import Test.QuickCheck.Monadic (forAllM, monadicIO, run)
import Test.Validity (GenValid (..))


spec :: Spec
spec = describe "Handshake" $ do
  context "using the plain mechanism" $
    it "should preserve the received start data" prop_handshakePreserveStartData


prop_handshakePreserveStartData :: Property
prop_handshakePreserveStartData = monadicIO $
  forAllM genStartDataAndResponses $
    \(startData, responses) -> run $ isStartDataPreserved startData responses


mkHandshakeReplys :: CoStartData -> CoTuneData -> ShortString -> [ByteString]
mkHandshakeReplys startData tuneData ignoredVhost =
  let asFrame = LBS.toStrict . BB.toLazyByteString . toBuilder . connFrame
      startFrame = asFrame $ CoStart startData
      tuneFrame = asFrame $ CoTune tuneData
      openOkFrame = asFrame $ CoOpenOk ignoredVhost
   in [startFrame, tuneFrame, BS.empty, openOkFrame]


genStartDataAndResponses :: Gen (CoStartData, [ByteString])
genStartDataAndResponses = do
  startData <- genStartData
  responses <- mkHandshakeReplys <$> (pure startData) <*> genValid <*> genValid
  pure (startData, responses)


genStartData :: Gen CoStartData
genStartData = do
  x <- genValid
  pure $ x {csMechanisms = LongString "PLAIN"}


isStartDataPreserved :: CoStartData -> [ByteString] -> IO Bool
isStartDataPreserved expected xs = do
  matched <- newIORef False
  (src, sink) <- linkedSrcAndSink xs
  let asBytes = LBS.toStrict . BB.toLazyByteString . toBuilder
      testContinuation got _ignored = do
        writeIORef matched $ asBytes got == asBytes expected
      shake = defaultHandshake sink src testContinuation
  runHandshake shake
  readIORef matched


connFrame :: Connection -> Frame Method
connFrame = Frame . Frame' . InnerFrame 0 . ModusConnection
