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
  Frame (..),
  Frame' (..),
) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (word16BE)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Validity (Validity (..))
import Data.Validity.ByteString ()
import Data.Word (Word16, Word8)
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


type instance FrameType Heartbeat = 8


type instance FrameType Method = 1


data Frame a where
  Frame ::
    ( n ~ FrameType a
    , KnownNat n
    , ToBuilder a BB.Builder
    , ParserOf a
    ) =>
    Frame' n a ->
    Frame a


instance ToBuilder (Frame a) BB.Builder where
  toBuilder (Frame x) = toBuilder x


instance
  ( KnownNat n
  , n ~ FrameType a
  , ToBuilder a BB.Builder
  , ParserOf a
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
  , ParserOf a
  ) =>
  ParserOf (Frame' n a)
  where
  parserOf = do
    _ <- A.word8 $ fromIntegral $ natVal @n Proxy
    Frame' <$> parserOf


instance
  ( ToBuilder a BB.Builder
  , KnownNat n
  ) =>
  ToBuilder (Frame' n a) BB.Builder
  where
  toBuilder (Frame' x) = BB.word8 (fromIntegral $ natVal @n Proxy) <> toBuilder x


data InnerFrame a = InnerFrame Word16 a
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ParserOf a => ParserOf (InnerFrame a) where
  parserOf = do
    channelId <- A.anyWord16be
    res <- A.anyWord32be >>= flip A.fixed parserOf
    _ <- A.word8 frameEnd
    pure $ InnerFrame channelId res


instance
  ( ToBuilder a BB.Builder
  ) =>
  ToBuilder (InnerFrame a) BB.Builder
  where
  toBuilder (InnerFrame channelId x) =
    let sub = BB.toLazyByteString $ toBuilder x
        subLength = fromIntegral $ LBS.length sub
     in word16BE channelId
          <> BB.word32BE subLength
          <> BB.lazyByteString sub
          <> BB.word8 frameEnd


frameEnd :: Word8
frameEnd = 0xCE


data Heartbeat = Heartbeat
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ParserOf Heartbeat where
  parserOf = pure Heartbeat


instance ToBuilder Heartbeat BB.Builder where
  toBuilder _ = toBuilder BS.empty


data ContentBody = Body !BS.ByteString
  deriving (Eq, Show, Generic)
  deriving anyclass (Validity)


instance ToBuilder ContentBody BB.Builder where
  toBuilder (Body b) = toBuilder b


instance ParserOf ContentBody where
  parserOf = Body <$> A.takeByteString


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


instance ParserOf ContentHdr where
  parserOf = ContentHdr <$> parserOf <*> parserOf <*> parserOf <*> parserOf


instance ToBuilder ContentHdr BB.Builder where
  toBuilder x =
    (toBuilder $ chClassId x)
      <> (toBuilder $ chWeight x)
      <> (toBuilder $ chBodySize x)
      <> (toBuilder $ chHeaders x)

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
--   (
--     ToBuilder a BB.Builder
--   , KnownNat n
--   ) =>
--   ToBuilder (Canal9 n a) BB.Builder
--   where
--   toBuilder (Canal9 x) = BB.word8 (fromIntegral $ natVal @n Proxy) <> toBuilder x
