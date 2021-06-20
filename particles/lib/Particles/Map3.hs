{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Particles.Map3 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Debug.Trace
import Linear.V2 (_x, _y)
import Particles.Types

{-# INLINE make #-}
make :: Int -> Double -> BoundingBox -> Particles2 -> ParticlesMap3
make bucketCapacity maxBucketSize bbox ps = runST $ do
    bucketsSizes <- VUM.new numberOfBuckets
    bucketsStorage <- VUM.unsafeNew $ numberOfBuckets * bucketCapacity
    fillBuckets bucketsSizes bucketsStorage
    frozenSizes <- VU.unsafeFreeze bucketsSizes
    frozenStorage <- VU.unsafeFreeze bucketsStorage
    return $ ParticlesMap3
        { map3BucketsSizes = frozenSizes
        , map3BucketsStorage = frozenStorage
        , map3Size = mapSize }
    where
        numberOfBuckets = mapSize * mapSize
        mapSize = getMapSize bbox maxBucketSize
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ particleIndex particle -> do
                let bIndex = bucketIndex $
                        bucketCoord bbox mapSize (particle ^. position)
                let beginningOfBucket = bucketCapacity * bIndex
                let bucket = VUM.slice
                        beginningOfBucket bucketCapacity storage
                bucketSize <- VUM.read sizes bIndex
                let particleOffset = bucketSize
                -- Put particle into bucket
                VUM.write bucket particleOffset particleIndex
                -- Increase size of the bucket
                VUM.write sizes bIndex (bucketSize + 1)

{-# INLINE getMapSize #-}
getMapSize :: BoundingBox -> Double -> Int
getMapSize bbox maxBucketSize =
    fromIntegral $ nextHighestPowerOf2 (ceiling $ biggerDim / maxBucketSize)
    where
        biggerDim = max width height
        width = bbox ^. right - bbox ^. left
        height = bbox ^. top - bbox ^. bottom

{-# INLINE nextHighestPowerOf2 #-}
nextHighestPowerOf2 :: Word32 -> Word32
nextHighestPowerOf2 x = x6 + 1 where
    x1 = x - 1
    x2 = x1 .|. unsafeShiftR x1 1
    x3 = x2 .|. unsafeShiftR x2 2
    x4 = x3 .|. unsafeShiftR x3 4
    x5 = x4 .|. unsafeShiftR x4 8
    x6 = x5 .|. unsafeShiftR x5 16

{-# INLINE bucketCoord #-}
bucketCoord :: BoundingBox -> Int -> Position -> BucketCoord
bucketCoord BoundingBox{..} mapSize pos = (row, col)
    where
        col = floor $ (pos ^. _x - _left) / bucketWidth
        row = floor $ (pos ^. _y - _bottom) / bucketHeight
        bucketWidth = bboxWidth / fromIntegral mapSize
        bucketHeight = bboxHeight / fromIntegral mapSize
        bboxWidth = _right - _left
        bboxHeight = _top - _bottom

{-# INLINE bucketIndex #-}
bucketIndex :: BucketCoord -> BucketIndex
bucketIndex (row, col) = fromIntegral $
    encodeMorton16 (fromIntegral col) (fromIntegral row)

{-# INLINE encodeMorton16 #-}
encodeMorton16 :: Word32 -> Word32 -> Word32
encodeMorton16 x y = xS .|. unsafeShiftL yS 1 where
    xS = separateBy1 x
    yS = separateBy1 y
    separateBy1 x0 = x4 where
        x1 = (x0 .|. unsafeShiftL x0 8) .&. 0x00FF00FF
        x2 = (x1 .|. unsafeShiftL x1 4) .&. 0x0F0F0F0F
        x3 = (x2 .|. unsafeShiftL x2 2) .&. 0x33333333
        x4 = (x3 .|. unsafeShiftL x3 1) .&. 0x55555555
