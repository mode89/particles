{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map2 where

import Control.Lens ((^.))
import Control.Monad.ST (runST, ST)
import qualified Data.Vector as V
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
    fillBuckets bbox bucketDim bucketsSizes bucketsStorage ps
    frozenSizes <- VU.unsafeFreeze bucketsSizes
    frozenStorage <- VU.unsafeFreeze bucketsStorage
    return $ ParticlesMap2
        { mapBucketsSizes = frozenSizes
        , mapBucketsStorage = frozenStorage
        , mapBoundingBox = bbox
        , mapBucketDim = bucketDim }
    where
        !numberOfBuckets = mapWidth * mapHeight
        !mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / bucketDim
        !mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / bucketDim
        !bucketDim = 50

fillBuckets :: BoundingBox
            -> BucketDim
            -> VUM.STVector s Int
            -> VUM.STVector s ParticleIndex
            -> Particles2
            -> ST s ()
fillBuckets bbox !dim sizes storage ps = do
    VU.iforM_ ps $ \particleIndex particle -> do
        let !bucketIndex_ = bucketIndex bbox dim (particle ^. position)
        let !beginningOfBucket = maxBucketSize * bucketIndex_
        let bucket = VUM.slice beginningOfBucket maxBucketSize storage
        bucketSize <- VUM.read sizes bucketIndex_
        let !particleOffset = bucketSize
        -- Put particle into bucket
        VUM.write bucket particleOffset particleIndex
        -- Increase size of the bucket
        VUM.write sizes bucketIndex_ (bucketSize + 1)

{-# INLINE bucketIndex #-}
bucketIndex :: BoundingBox -> BucketDim -> Position -> BucketIndex
bucketIndex BoundingBox{..} !bucketDim pos =
    indexFromRowAndColumn columns row column
    where
        !column = floor $ (pos ^. _x - _left) / bucketDim
        !row = floor $ (pos ^. _y - _bottom) / bucketDim
        !columns = ceiling $ (_right - _left) / bucketDim

{-# INLINE indexFromRowAndColumn #-}
indexFromRowAndColumn :: Int -> Int -> Int -> BucketIndex
indexFromRowAndColumn !width !row !column = column + row * width
