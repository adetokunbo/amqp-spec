{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Protocol.AMQP.Frame (
  -- * data types
  Heartbeat (..),
  ContentBody (..),
  ContentHdr (..),
  InnerFrame (..),
) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (word16BE)
import qualified Data.ByteString.Builder as BB
import Data.Proxy (Proxy (..))
import Data.Validity (Validity (..))
import Data.Validity.ByteString ()
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Protocol.AMQP.Attoparsec as A
import Protocol.AMQP.Elementary (
  LongLongInt,
  ParserOf (..),
  ShortInt,
  ToBuilder (..),
 )
import Protocol.AMQP.Translated (BasicHdr, Method)


data Frame a where
  Frame ::
    ( n ~ FrameType a
    , KnownNat n
    , ToBuilder a BB.Builder
    , ToFrameBuilder a
    , FrameParser a
    ) =>
    Frame' n a ->
    Frame a


instance ToBuilder (Frame a) BB.Builder where
  toBuilder (Frame x) = toBuilder x


instance
  ( KnownNat n
  , n ~ FrameType a
  , ToFrameBuilder a
  , ToFrameBuilder a
  , FrameParser a
  ) =>
  ParserOf (Frame a)
  where
  parserOf = Frame <$> parserOf


data Frame' (n :: Nat) a :: * where
  Frame' :: InnerFrame a -> Frame' (FrameType a) a


type family FrameType (a :: *) :: Nat


instance
  ( n ~ FrameType a
  , KnownNat n
  , FrameParser a
  ) =>
  ParserOf (Frame' n a)
  where
  parserOf = do
    _ <- A.word8 $ fromIntegral $ natVal @n Proxy
    Frame' <$> parserOf


instance
  ( ToFrameBuilder a
  , ToBuilder a BB.Builder
  , KnownNat n
  ) =>
  ToBuilder (Frame' n a) BB.Builder
  where
  toBuilder (Frame' x) = BB.word8 (fromIntegral $ natVal @n Proxy) <> toBuilder x


data InnerFrame a = InnerFrame Word16 a
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance FrameParser a => ParserOf (InnerFrame a) where
  parserOf = do
    channelId <- A.anyWord16be
    size <- A.anyWord32be
    res <- frameParser size
    _ <- A.word8 frameEnd
    pure $ InnerFrame channelId res


instance
  ( ToFrameBuilder a
  , ToBuilder a BB.Builder
  ) =>
  ToBuilder (InnerFrame a) BB.Builder
  where
  toBuilder (InnerFrame channelId x) =
    word16BE channelId
      <> toFrameBuilder x
      <> BB.word8 frameEnd


frameEnd :: Word8
frameEnd = 0xCE


class FrameParser a where
  frameParser :: Word32 -> A.Parser a


class ToBuilder a BB.Builder => ToFrameBuilder a where
  toFrameBuilder :: a -> BB.Builder


data Heartbeat = Heartbeat
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance FrameParser Heartbeat where
  frameParser x = A.take (fromIntegral x) >> pure Heartbeat


instance ToBuilder Heartbeat BB.Builder where
  toBuilder _ = toBuilder BS.empty


instance ToFrameBuilder Heartbeat where
  toFrameBuilder x = BB.word32BE 0 <> toBuilder x


data ContentBody = Body !BS.ByteString
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ToBuilder ContentBody BB.Builder where
  toBuilder (Body b) = toBuilder b


instance FrameParser ContentBody where
  frameParser x = Body <$> A.take (fromIntegral x)


instance ToFrameBuilder ContentBody where
  toFrameBuilder (Body b) = BB.word32BE (fromIntegral $ BS.length b) <> toBuilder b


instance FrameParser Method where
  frameParser _ = parserOf


instance ToFrameBuilder Method where
  toFrameBuilder = toBuilder


{-- How to model this best ?
There are specific values for chClassId, so possibly disallow invalid ones ?
Also, when chClassId is not one value, chHeaders is always a null value...
-}
data ContentHdr = ContentHdr
  { chClassId :: !ShortInt
  , chWeight :: !ShortInt
  , chBodySize :: !LongLongInt
  , chHeaders :: !BasicHdr
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance FrameParser ContentHdr where
  frameParser _ = ContentHdr <$> parserOf <*> parserOf <*> parserOf <*> parserOf


instance ToBuilder ContentHdr BB.Builder where
  toBuilder x =
    (toBuilder $ chClassId x)
      <> (toBuilder $ chWeight x)
      <> (toBuilder $ chBodySize x)
      <> (toBuilder $ chHeaders x)


instance ToFrameBuilder ContentHdr where
  toFrameBuilder = toBuilder

-- newtype Payload (n :: Nat) a = Payload a

-- instance (KnownNat n, ParserOf a) => ParserOf (Payload n a) where
--   parserOf = do
--     _ <- A.word8 $ fromIntegral $ natVal @n Proxy
--     Payload <$> parserOf

-- instance (KnownNat n, ToBuilder a BB.Builder) => ToBuilder (Payload n a) BB.Builder where
--   toBuilder (Payload x) = BB.word8 (fromIntegral $ natVal @n Proxy) <> toBuilder x

-- data Canal9 (n :: Nat) a = Canal9 (InnerFrame a)

-- instance (KnownNat n, FrameParser a) => ParserOf (Canal9 n a) where
--   parserOf = do
--     _ <- A.word8 $ fromIntegral $ natVal @n Proxy
--     Canal9 <$> parserOf

-- instance
--   ( ToFrameBuilder a
--   , ToBuilder a BB.Builder
--   , KnownNat n
--   ) =>
--   ToBuilder (Canal9 n a) BB.Builder
--   where
--   toBuilder (Canal9 x) = BB.word8 (fromIntegral $ natVal @n Proxy) <> toBuilder x
