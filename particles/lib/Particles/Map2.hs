{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map2 where

import Control.Lens ((^.))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (_x, _y)
import Particles.Types

update :: BoundingBox -> Particles2 -> ParticlesMap2 -> IO ParticlesMap2
update bbox ps pmap = do
    nextMap <- reset bbox pmap
    fillBuckets nextMap ps
    return nextMap

reset :: BoundingBox -> ParticlesMap2 -> IO ParticlesMap2
reset bbox pmap = do
    if bbox /= mapBoundingBox pmap
        then initialize bbox
        else do
            resetBuckets $ mapBuckets pmap
            return pmap

resetBuckets :: MapBuckets -> IO ()
resetBuckets (MapBuckets buckets) = do
    V.forM_ buckets $ \MapBucket{..} -> do
        writeIORef bucketParticlesNum 0

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

fillBuckets :: ParticlesMap2 -> Particles2 -> IO ()
fillBuckets ParticlesMap2{..} ps = do
    VU.iforM_ ps $ \particleIndex Particle{..} -> do
        let (MapBuckets buckets) = mapBuckets
        let MapBucket{..} = V.unsafeIndex buckets $
                bucketIndex mapBoundingBox mapBucketDim _position
        bucketParticlesNumValue <- readIORef bucketParticlesNum
        VUM.write bucketParticlesIndices
            bucketParticlesNumValue particleIndex
        writeIORef bucketParticlesNum $ bucketParticlesNumValue + 1

bucketIndex :: BoundingBox -> BucketDim -> Position -> BucketIndex
bucketIndex BoundingBox{..} bucketDim pos =
    indexFromRowAndColumn columns row column
    where
        column = floor $ (pos ^. _x - _left) / bucketDim
        row = floor $ (pos ^. _y - _bottom) / bucketDim
        columns = ceiling $ (_right - _left) / bucketDim

indexFromRowAndColumn :: Int -> Int -> Int -> BucketIndex
indexFromRowAndColumn width row column = column + row * width
