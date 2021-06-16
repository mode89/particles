{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Particles.Map2 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types

{-# INLINE maxBucketSize #-}
maxBucketSize :: Int
maxBucketSize = 100

{-# INLINE mapBucketDim #-}
mapBucketDim :: Fractional a => a
mapBucketDim = 50.0

{-# INLINE make #-}
make :: BoundingBox -> Particles2 -> ParticlesMap2
make bbox ps = runST $ do
    bucketsSizes <- VUM.new numberOfBuckets
    bucketsStorage <- VUM.unsafeNew $ numberOfBuckets * maxBucketSize
    fillBuckets bucketsSizes bucketsStorage
    frozenSizes <- VU.unsafeFreeze bucketsSizes
    frozenStorage <- VU.unsafeFreeze bucketsStorage
    return $ ParticlesMap2
        { mapBucketsSizes = frozenSizes
        , mapBucketsStorage = frozenStorage
        , mapWidth = width
        , mapHeight = height }
    where
        numberOfBuckets = width * height
        width = ceiling $ (bbox ^. right - bbox ^. left) / mapBucketDim
        height = ceiling $ (bbox ^. top - bbox ^. bottom) / mapBucketDim
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ particleIndex particle -> do
                let bucketIndex_ = bucketIndex bbox (particle ^. position)
                let beginningOfBucket = maxBucketSize * bucketIndex_
                let bucket = VUM.slice
                        beginningOfBucket maxBucketSize storage
                bucketSize <- VUM.read sizes bucketIndex_
                let particleOffset = bucketSize
                -- Put particle into bucket
                VUM.write bucket particleOffset particleIndex
                -- Increase size of the bucket
                VUM.write sizes bucketIndex_ (bucketSize + 1)

{-# INLINE bucketIndex #-}
bucketIndex :: BoundingBox -> Position -> BucketIndex
bucketIndex BoundingBox{..} pos =
    indexFromRowAndColumn columns row column
    where
        column = floor $ (pos ^. _x - _left) / mapBucketDim
        row = floor $ (pos ^. _y - _bottom) / mapBucketDim
        columns = ceiling $ (_right - _left) / mapBucketDim

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
            let beginningOfBucket = bIndex * maxBucketSize
                bucketSize = mapBucketsSizes VU.! bIndex
            in VU.slice beginningOfBucket bucketSize mapBucketsStorage
        (bRow, bCol) = rowAndColumnFromIndex pBucketIndex mapWidth

{-# INLINE neighbourBuckets #-}
neighbourBuckets :: Int -> Int -> Int -> Int -> VU.Vector BucketIndex
neighbourBuckets mapWidth mapHeight bRow bCol
    = VU.map bucketIndex_
    . VU.filter insideBoundingBox
    . VU.map absoluteBucketCoord
    $ neighbourOffsets
    where
        bucketIndex_ = uncurry $ indexFromRowAndColumn mapWidth
        insideBoundingBox (r, c)
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
