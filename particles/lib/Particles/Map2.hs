{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map2 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types

{-# INLINE make #-}
make :: BucketCapacity
     -> CellSize
     -> BoundingBox
     -> Particles2
     -> ParticlesMap2
make bCapacity cSize bbox@BoundingBox{..} ps = runST $ do
    bucketsSizes <- VUM.new numberOfBuckets
    bucketsStorage <- VUM.unsafeNew $ numberOfBuckets * bCapacity
    fillBuckets bucketsSizes bucketsStorage
    frozenSizes <- VU.unsafeFreeze bucketsSizes
    frozenStorage <- VU.unsafeFreeze bucketsStorage
    return $ ParticlesMap2
        { mapBucketsSizes = frozenSizes
        , mapBucketsStorage = frozenStorage
        , mapBucketCapacity = bCapacity
        , mapWidth = width
        , mapHeight = height }
    where
        numberOfBuckets = width * height
        !width = ceiling $ bboxWidth / cSize
        !height = ceiling $ bboxHeight / cSize
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ particleIndex particle -> do
                let bIndex = bucketIndex bbox cSize (particle ^. position)
                let beginningOfBucket = bCapacity * bIndex
                let bucket = VUM.slice
                        beginningOfBucket bCapacity storage
                bucketSize <- VUM.read sizes bIndex
                let particleOffset = bucketSize
                -- Put particle into bucket
                VUM.write bucket particleOffset particleIndex
                -- Increase size of the bucket
                VUM.write sizes bIndex (bucketSize + 1)

{-# INLINE bucketIndex #-}
bucketIndex :: BoundingBox -> CellSize -> Position -> BucketIndex
bucketIndex BoundingBox{..} cSize pos =
    indexFromRowAndColumn columns row column
    where
        column = floor $ (pos ^. _x - bboxLeft) / cSize
        row = floor $ (pos ^. _y - bboxBottom) / cSize
        columns = ceiling $ bboxWidth / cSize

{-# INLINE indexFromRowAndColumn #-}
indexFromRowAndColumn :: Int -> Int -> Int -> BucketIndex
indexFromRowAndColumn width row column = column + row * width

{-# INLINE neighbourParticles #-}
neighbourParticles :: ParticlesMap2
                   -> BucketIndex
                   -> VU.Vector ParticleIndex
neighbourParticles ParticlesMap2{..} pBucketIndex
    = VU.concatMap particlesInsideBucket
    $ neighbourBuckets mapWidth mapHeight bRow bCol
    where
        particlesInsideBucket bIndex =
            let beginningOfBucket = bIndex * mapBucketCapacity
                bucketSize = mapBucketsSizes VU.! bIndex
            in VU.slice beginningOfBucket bucketSize mapBucketsStorage
        (bRow, bCol) = rowAndColumnFromIndex pBucketIndex mapWidth

{-# INLINE neighbourBuckets #-}
neighbourBuckets :: Int -> Int -> Int -> Int -> VU.Vector BucketIndex
neighbourBuckets mapWidth mapHeight !bRow !bCol
    = VU.map bucketIndex_
    . VU.filter insideBoundingBox
    . VU.map absoluteBucketCoord
    $ neighbourOffsets
    where
        bucketIndex_ (r, c) = indexFromRowAndColumn mapWidth r c
        insideBoundingBox (!r, !c)
             = (r >= 0)
            && (r < mapHeight)
            && (c >= 0)
            && (c < mapWidth)
        absoluteBucketCoord (r, c) =
            ( bRow + r - 1
            , bCol + c - 1 )
        neighbourOffsets = VU.generate 9 (`rowAndColumnFromIndex` 3)

{-# INLINE rowAndColumnFromIndex #-}
rowAndColumnFromIndex :: BucketIndex -> Int -> (Int, Int)
rowAndColumnFromIndex index width = index `divMod` width

-- Tests for optimizing core

-- testNP :: ParticlesMap2 -> BucketIndex -> Int
-- testNP pmap bIndex = VU.sum $ neighbourParticles pmap bIndex

-- testNB :: Int -> Int -> Int -> Int -> Int
-- testNB w h r c = VU.sum $ neighbourBuckets w h r c
