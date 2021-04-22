{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Map where

import Control.Lens ((^.))
import qualified Data.Vector as V
import Linear.V2 (V2(..), _x, _y)
import Particles.Types

makeParticlesMap :: BoundingBox -> Particles -> ParticlesMap
makeParticlesMap bbox particles =
    ParticlesMap { buckets = sortedParticles
                 , boundingBox = bbox
                 , width = mapWidth
                 , bucketDim = mapBucketDim }
    where
        sortedParticles =
            V.accum appendParticle emptyBuckets labeledParticles
        appendParticle ps p = p : ps
        emptyBuckets = V.replicate numberOfBuckets []
        labeledParticles = labelParticle <$> particles
        labelParticle p =
            (particleBucketIndex bbox mapBucketDim $ p ^. position, p)
        numberOfBuckets = mapWidth * mapHeight
        mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / mapBucketDim
        mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / mapBucketDim
        mapBucketDim = 50

particlesInsideBucket :: ParticlesMap -> BucketIndex -> Particles
particlesInsideBucket ParticlesMap{..} = V.unsafeIndex buckets

particleBucketIndex :: BoundingBox -> BucketDim -> Position -> BucketIndex
particleBucketIndex BoundingBox{..} bucketDim pos =
    indexFromRowAndColumn columns row column
    where
        column = floor $ (pos ^. _x - _left) / bucketDim
        row = floor $ (pos ^. _y - _bottom) / bucketDim
        columns = ceiling $ (_right - _left) / bucketDim

neighbourParticles :: ParticlesMap -> Particle -> Particles
neighbourParticles pmap@ParticlesMap{..} particle =
    concat $ particleBucketNeighbours : neighbourBucketParticles
    where
        particleBucketNeighbours = filter (/= particle)
                                 . particlesInsideBucket pmap
                                 $ bucketIndex
        neighbourBucketParticles = fmap (particlesInsideBucket pmap)
                                 . neighbourBuckets pmap
                                 $ bucketIndex
        bucketIndex = particleBucketIndex boundingBox bucketDim $ pos
        pos = particle ^. position

neighbourBuckets :: ParticlesMap -> BucketIndex -> [BucketIndex]
neighbourBuckets ParticlesMap{buckets, width} index =
      filter (/= index)
    . fmap (uncurry $ indexFromRowAndColumn width)
    $ rowsAndCols
    where
        rowsAndCols = [(r, c) | r <- rows, c <- cols]
        rows = filter (>= 0) . filter (< height) $ [row - 1, row, row + 1]
        cols = filter (>= 0) . filter (< width) $ [col - 1, col, col + 1]
        row = index `div` width
        col = index `mod` width
        height = length buckets `div` width

indexFromRowAndColumn :: Int -> Int -> Int -> BucketIndex
indexFromRowAndColumn width row column = column + row * width
