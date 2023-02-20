{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified AMQP.ElementarySpec as Elementary
import qualified AMQP.FieldValueSpec as FieldValue
import qualified AMQP.FrameSpec as Frame
import qualified AMQP.TranslatedSpec as Translated
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec $ do
    Elementary.spec
    Frame.spec
    FieldValue.spec
    Translated.spec
