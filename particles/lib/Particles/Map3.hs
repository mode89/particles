{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map3 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types hiding (ParticlesMap)
import Particles.Map3.Types

{-# INLINE newMMapUnsafe #-}
newMMapUnsafe :: BucketCapacity -> MapSize -> IO MParticlesMap
newMMapUnsafe bCapacity size =
    MParticlesMap <$> VUM.unsafeNew numberOfBuckets
                  <*> VUM.unsafeNew (numberOfBuckets * bCapacity)
                  <*> return bCapacity
                  <*> return size
    where
        numberOfBuckets = size * size

{-# INLINE update #-}
update :: BucketCapacity
       -> BucketSize
       -> BoundingBox
       -> Particles2
       -> MParticlesMap
       -> IO MParticlesMap
update !bCapacity !maxBucketSize bbox@BoundingBox{..} ps prevMap = do
    -- Create new map if bounding has changed
    let mapSize = getMapSize bbox maxBucketSize
    nextMap <- if mapSize == mapSizeM prevMap
               then return prevMap
               else newMMapUnsafe bCapacity mapSize
    let sizes = mapBucketsSizesM nextMap
    let storage = mapBucketsStorageM nextMap
    -- Clear map
    VUM.set sizes 0
    let !bWidth = bboxWidth / fromIntegral mapSize
    let !bHeight = bboxHeight / fromIntegral mapSize
    VU.iforM_ ps $ \ particleIndex particle -> do
        let bIndex = bucketIndex $
                bucketCoord bbox bWidth bHeight (particle ^. position)
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
     -> ParticlesMap
make bucketCapacity maxBucketSize bbox@BoundingBox{..} ps = runST $ do
    bucketsSizes <- VUM.new numberOfBuckets
    bucketsStorage <- VUM.unsafeNew $ numberOfBuckets * bucketCapacity
    fillBuckets bucketsSizes bucketsStorage
    frozenSizes <- VU.unsafeFreeze bucketsSizes
    frozenStorage <- VU.unsafeFreeze bucketsStorage
    return $ ParticlesMap
        { mapBucketsSizes = frozenSizes
        , mapBucketsStorage = frozenStorage
        , mapBucketCapacity = bucketCapacity
        , mapSize = mapSize }
    where
        numberOfBuckets = mapSize * mapSize
        mapSize = getMapSize bbox maxBucketSize
        bWidth = bboxWidth / fromIntegral mapSize
        bHeight = bboxHeight / fromIntegral mapSize
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ particleIndex particle -> do
                let bIndex = bucketIndex $ bucketCoord bbox bWidth bHeight
                        (particle ^. position)
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
bucketCoord :: BoundingBox -> Double -> Double -> Position -> BucketCoord
bucketCoord BoundingBox{..} bWidth bHeight pos = (row, col)
    where
        col = floor $ (pos ^. _x - bboxLeft) / bWidth
        row = floor $ (pos ^. _y - bboxBottom) / bHeight

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

{-# INLINE neighbourBuckets #-}
neighbourBuckets :: MapSize -> BucketCoord -> VU.Vector BucketIndex
neighbourBuckets mapSize (bRow, bCol)
    = VU.map bucketIndex
    . VU.filter insideBoundingBox
    . VU.map absoluteBucketCoord
    $ neighbourOffsets
    where
        insideBoundingBox (r, c)
             = (r >= 0)
            && (r < mapSize)
            && (c >= 0)
            && (c < mapSize)
        absoluteBucketCoord (r, c) =
            ( bRow + r - 1
            , bCol + c - 1 )
        neighbourOffsets = VU.generate 9 (`divMod` 3)

-- testNB :: MapSize -> BucketCoord -> Int
-- testNB !mapSize (!r, !c) = VU.sum $ neighbourBuckets mapSize (r, c)

{-# INLINE neighbourParticles #-}
neighbourParticles :: ParticlesMap
                   -> BucketCoord
                   -> VU.Vector ParticleIndex
neighbourParticles ParticlesMap{..} bCoord
    = VU.concatMap particlesInsideBucket
    $ neighbourBuckets mapSize bCoord
    where
        particlesInsideBucket bIndex =
            let beginningOfBucket = bIndex * mapBucketCapacity
                bucketSize = mapBucketsSizes VU.! bIndex
            in VU.slice beginningOfBucket bucketSize mapBucketsStorage

-- testNP :: ParticlesMap -> BucketCoord -> Int
-- testNP pmap (!bRow, !bCol) = VU.sum $ neighbourParticles pmap (bRow, bCol)
