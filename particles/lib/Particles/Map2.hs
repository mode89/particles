{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map2 where

import Control.Lens ((^.))
import Data.IORef (newIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Particles.Types

initialize :: BoundingBox -> IO ParticlesMap2
initialize bbox = do
    buckets <- V.replicateM numberOfBuckets newBucket
    return $ ParticlesMap2
        { mapBuckets = MapBuckets $ buckets
        , mapBoundingBox = bbox
        , mapBucketDim = bucketDim }
    where
        numberOfBuckets = mapWidth * mapHeight
        mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / bucketDim
        mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / bucketDim
        bucketDim = 50

newBucket :: IO MapBucket
newBucket = do
    size <- newIORef 0
    indices <- VUM.new 100
    return $ MapBucket size indices
