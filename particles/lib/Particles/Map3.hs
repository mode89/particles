{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map3 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.Bits
import Debug.Trace
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types
import Particles.Map3.Types as M3

{-# INLINE make #-}
make
    :: BucketCapacity
    -> CellSize
    -> BoundingBox
    -> Particles2
    -> M3.ParticlesMap
make bCapacity cSize bbox ps = unsafeUpdate bCapacity cSize bbox ps $
    M3.ParticlesMap
        { mapBucketsSizes = VU.empty
        , mapBucketsStorage = VU.empty
        , mapBucketCapacity = 0
        , mapSize = 0 }

{-# INLINE unsafeUpdate #-}
unsafeUpdate
    :: BucketCapacity
    -> CellSize
    -> BoundingBox
    -> Particles2
    -> M3.ParticlesMap
    -> M3.ParticlesMap
unsafeUpdate !bCapacity !cSize bbox ps pmapPrev = runST $ do
    (sizes, storage) <-
        if needNewMap
        then (,) <$> VUM.unsafeNew numberOfBuckets
                 <*> VUM.unsafeNew (numberOfBuckets * bCapacity)
        else (,) <$> VU.unsafeThaw (M3.mapBucketsSizes pmapPrev)
                 <*> VU.unsafeThaw (M3.mapBucketsStorage pmapPrev)
    -- Clear map
    VUM.set sizes 0
    fillBuckets sizes storage
    frozenSizes <- VU.unsafeFreeze sizes
    frozenStorage <- VU.unsafeFreeze storage
    return $ M3.ParticlesMap
        { mapBucketsSizes = frozenSizes
        , mapBucketsStorage = frozenStorage
        , mapBucketCapacity = bCapacity
        , mapSize = mapSize }
    where
        needNewMap = (requiredMapSize > M3.mapSize pmapPrev)
                  || (bCapacity > M3.mapBucketCapacity pmapPrev)
        requiredMapSize = getMapSize bbox cSize
        numberOfBuckets = mapSize * mapSize
        !mapSize = max requiredMapSize (M3.mapSize pmapPrev)
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ pIndex particle -> do
                let bIndex = bucketIndex $
                        bucketCoord bbox cSize (particle ^. position)
                let beginningOfBucket = bCapacity * bIndex
                let bucket = VUM.slice
                        beginningOfBucket bCapacity storage
                bSize <- VUM.read sizes bIndex
                let particleOffset = bSize
                -- Put particle into bucket
                VUM.write bucket particleOffset pIndex
                -- Increase size of the bucket
                VUM.write sizes bIndex (bSize + 1)

{-# INLINE getMapSize #-}
getMapSize :: BoundingBox -> Double -> Int
getMapSize BoundingBox{..} cellSize
    = fromIntegral
    . nextHighestPowerOf2
    . ceiling
    $ biggerDim / cellSize
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
bucketCoord :: BoundingBox -> CellSize -> Position -> BucketCoord
bucketCoord BoundingBox{..} cellSize pos = (row, col)
    where
        col = floor $ (pos ^. _x - bboxLeft) / cellSize
        row = floor $ (pos ^. _y - bboxBottom) / cellSize

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
neighbourBuckets mapSize (!bRow, !bCol)
    = VU.map bucketIndex
    . VU.filter insideBoundingBox
    . VU.map absoluteBucketCoord
    $ neighbourOffsets
    where
        insideBoundingBox (!r, !c)
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
neighbourParticles :: M3.ParticlesMap
                   -> BucketCoord
                   -> VU.Vector ParticleIndex
neighbourParticles M3.ParticlesMap{..} bCoord
    = VU.concatMap particlesInsideBucket
    $ neighbourBuckets mapSize bCoord
    where
        particlesInsideBucket bIndex =
            let beginningOfBucket = bIndex * mapBucketCapacity
                bucketSize = mapBucketsSizes VU.! bIndex
            in VU.slice beginningOfBucket bucketSize mapBucketsStorage

-- testNP :: ParticlesMap -> BucketCoord -> Int
-- testNP pmap (!bRow, !bCol) = VU.sum $ neighbourParticles pmap (bRow, bCol)
