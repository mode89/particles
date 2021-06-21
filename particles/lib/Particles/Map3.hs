{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Particles.Map3 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types

{-# INLINE newMMapUnsafe #-}
newMMapUnsafe :: BucketCapacity -> MapSize -> IO MParticlesMap3
newMMapUnsafe bCapacity size =
    MParticlesMap3 <$> VUM.unsafeNew numberOfBuckets
                   <*> VUM.unsafeNew (numberOfBuckets * bCapacity)
                   <*> return size
    where
       numberOfBuckets = size * size

{-# INLINE update #-}
update :: BucketCapacity
       -> BucketSize
       -> BoundingBox
       -> Particles2
       -> MParticlesMap3
       -> IO MParticlesMap3
update bCapacity maxBucketSize bbox ps prevMap = do
    -- Create new map if bounding has changed
    let mapSize = getMapSize bbox maxBucketSize
    nextMap <- if mapSize == mMap3Size prevMap
               then return prevMap
               else newMMapUnsafe bCapacity mapSize
    let sizes = mMap3BucketsSizes nextMap
    let storage = mMap3BucketsStorage nextMap
    -- Clear map
    VUM.set sizes 0
    VU.iforM_ ps $ \ particleIndex particle -> do
        let bIndex = bucketIndex $
                bucketCoord bbox mapSize (particle ^. position)
        let beginningOfBucket = bCapacity * bIndex
        let bucket = VUM.slice
                beginningOfBucket bCapacity storage
        bucketSize <- VUM.read sizes bIndex
        let particleOffset = bucketSize
        -- Put particle into bucket
        VUM.write bucket particleOffset particleIndex
        -- Increase size of the bucket
        VUM.write sizes bIndex (bucketSize + 1)
    return nextMap

{-# INLINE make #-}
make :: BucketCapacity
     -> BucketSize
     -> BoundingBox
     -> Particles2
     -> ParticlesMap3
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
getMapSize BoundingBox{..} maxBucketSize
    = fromIntegral
    . nextHighestPowerOf2
    . ceiling
    $ biggerDim / maxBucketSize
    where
        biggerDim = max bboxWidth bboxHeight

{-# INLINE nextHighestPowerOf2 #-}
nextHighestPowerOf2 :: Int -> Int
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
        col = floor $ (pos ^. _x - bboxLeft) / bucketWidth
        row = floor $ (pos ^. _y - bboxBottom) / bucketHeight
        bucketWidth = bboxWidth / fromIntegral mapSize
        bucketHeight = bboxHeight / fromIntegral mapSize

{-# INLINE bucketIndex #-}
bucketIndex :: BucketCoord -> BucketIndex
bucketIndex (row, col) = fromIntegral $
    encodeMorton16 (fromIntegral col) (fromIntegral row)

{-# INLINE encodeMorton16 #-}
encodeMorton16 :: Int -> Int -> Int
encodeMorton16 x y = xS .|. unsafeShiftL yS 1 where
    xS = separateBy1 x
    yS = separateBy1 y
    separateBy1 x0 = x4 where
        x1 = (x0 .|. unsafeShiftL x0 8) .&. 0x00FF00FF
        x2 = (x1 .|. unsafeShiftL x1 4) .&. 0x0F0F0F0F
        x3 = (x2 .|. unsafeShiftL x2 2) .&. 0x33333333
        x4 = (x3 .|. unsafeShiftL x3 1) .&. 0x55555555
