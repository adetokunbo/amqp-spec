{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.Bits (
  -- * data types
  buildWithPrefix,
  CanBuild (..),
  IndexedProp,
  anyIndexedPropMb,

  -- * functions
) where

import Data.Bits
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as BB
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Protocol.AMQP.Attoparsec as A
import Protocol.AMQP.FieldValue


buildWithPrefix :: [Maybe CanBuild] -> BB.Builder
buildWithPrefix xs =
  let (asBits, _, asBytes) = foldl' canStep (0, 0, mempty) xs
   in word16BE asBits <> asBytes


data CanBuild = forall a. ToBuilder a BB.Builder => CanBuild a


canStep :: (Word16, Int, BB.Builder) -> Maybe CanBuild -> (Word16, Int, BB.Builder)
canStep (acc, pos, builder) Nothing = (acc, pos + 1, builder)
canStep (acc, pos, builder) (Just (CanBuild b)) = (acc `setBit` pos, pos + 1, builder <> (toBuilder b))


anyIndexedPropMb ::
  forall (n :: Nat) a b.
  ( KnownNat n
  , n ~ IndexedProp a
  , ParserOf a
  , Bits b
  ) =>
  b ->
  A.Parser (Maybe a)
anyIndexedPropMb x | testBit x (fromIntegral $ natVal @n Proxy) = fmap Just $ parserOf
anyIndexedPropMb _ = pure Nothing


type family IndexedProp (a :: *) :: Nat
