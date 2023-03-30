{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Protocol.AMQP.TrackedIds (
  TrackedIds (..),
  releaseId,
  releaseIdIO,
  nextAvailableId,
  nextAvailableIdIO,
) where

import Control.Monad (mfilter)
import Data.Bits
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word


nextAvailableIdIO :: IORef TrackedIds -> IO (Maybe Int)
nextAvailableIdIO ref = do
  nextAvailableId <$> readIORef ref >>= \case
    Nothing -> pure Nothing
    Just (anId, nextTracker) -> do
      writeIORef ref nextTracker
      pure $ Just anId


releaseIdIO :: IORef TrackedIds -> Int -> IO Bool
releaseIdIO ref anId = do
  (flip releaseId anId) <$> readIORef ref >>= \case
    Nothing -> pure False
    Just nextTracker -> do
      writeIORef ref nextTracker
      pure True


data TrackedIds = TrackedIds
  { tiAllocs :: ![Allocation]
  , tiMaximum :: !Int
  }
  deriving (Eq, Show)


releaseId :: TrackedIds -> Int -> Maybe TrackedIds
releaseId ti@TrackedIds {tiAllocs = as} anId =
  let (chunkIndex, bitIndex) = divMod anId 64
      chunkMb = lookup chunkIndex $ zip [0 ..] as
      insert' y = ti {tiAllocs = take chunkIndex as <> [y] <> drop (chunkIndex + 1) as}
      updateChunk = freeNth (fromIntegral bitIndex)
   in fmap insert' $ updateChunk =<< chunkMb


nextAvailableId :: TrackedIds -> Maybe (Int, TrackedIds)
nextAvailableId ti@TrackedIds {tiAllocs, tiMaximum} =
  let setAllocs as = ti {tiAllocs = as}
      seekChunkIndex (idx, a) (mbIndex, as) = case (mbIndex, nextFromAlloc a) of
        (Nothing, Just (bitIndex, x)) -> (Just (idx, bitIndex), x : as)
        (Nothing, Nothing) -> (Nothing, a : as)
        (Just found, _) -> (Just found, a : as)
      computeId bitIndex chunkIndex as =
        let theId = chunkIndex * 64 + bitIndex
         in if theId > tiMaximum then Nothing else Just (theId, as)
      orMaybeExtend (Nothing, as) = computeId 0 (length as) $ setAllocs $ as <> [freshAlloc]
      orMaybeExtend (Just (idx, bitIndex), as) = computeId bitIndex idx $ setAllocs as
   in orMaybeExtend $ foldr seekChunkIndex (Nothing, []) $ zip ([0 ..]) tiAllocs


data Allocation = Allocation
  { allocBlock :: !Word64
  , allocFreed :: !Word8
  , allocLastSet :: !Word8
  }
  deriving (Eq, Show)


freshAlloc :: Allocation
freshAlloc = Allocation 0 0 0


freeNth :: Word8 -> Allocation -> Maybe Allocation
freeNth freedBit alloc@Allocation {allocBlock, allocFreed} =
  let bitIndex = fromIntegral freedBit
      withUpdate =
        alloc
          { allocFreed = 1 + allocFreed
          , allocBlock = clearBit allocBlock bitIndex
          }
      wasSet = testBit allocBlock bitIndex
   in if wasSet then Just withUpdate else Nothing


nextFromAlloc :: Allocation -> Maybe (Int, Allocation)
nextFromAlloc a@Allocation {allocFreed, allocLastSet} =
  let setAllocBit x =
        a
          { allocBlock = setBit (allocBlock a) x
          , allocLastSet = fromIntegral x
          }
      justUpdateBit x = Just (x, setAllocBit x)
      updateAnUnsetBit = do
        lowestUnset <- mfilter (/= 64) $ findUnsetBit $ allocBlock a
        justUpdateBit lowestUnset
   in case (allocFreed == 0, allocLastSet == 64) of
        (True, True) -> Nothing
        (False, True) -> updateAnUnsetBit
        (_, False) -> justUpdateBit $ fromIntegral $ allocLastSet + 1


findUnsetBit :: Word64 -> Maybe Int
findUnsetBit w = checkIfUnset 0
  where
    checkIfUnset 65 = Nothing
    checkIfUnset bitIndex | not (testBit w bitIndex) = Just bitIndex
    checkIfUnset bitIndex = checkIfUnset (bitIndex + 1)
