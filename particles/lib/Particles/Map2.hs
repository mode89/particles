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
        , mapBoundingBox = bbox
        , mapBucketDim = bucketDim }
    where
        numberOfBuckets = mapWidth * mapHeight
        mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / bucketDim
        mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / bucketDim
        bucketDim = 50
        fillBuckets sizes storage =
            VU.iforM_ ps $ \ particleIndex particle -> do
                let bucketIndex_ = bucketIndex
                        bbox bucketDim (particle ^. position)
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
bucketIndex :: BoundingBox -> BucketDim -> Position -> BucketIndex
bucketIndex BoundingBox{..} bucketDim pos =
    indexFromRowAndColumn columns row column
    where
        column = floor $ (pos ^. _x - _left) / bucketDim
        row = floor $ (pos ^. _y - _bottom) / bucketDim
        columns = ceiling $ (_right - _left) / bucketDim

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
        mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / mapBucketDim
        mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / mapBucketDim
        bbox = mapBoundingBox
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
