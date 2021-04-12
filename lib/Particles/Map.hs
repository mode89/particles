{-# LANGUAGE RecordWildCards #-}

module Particles.Map where

import Control.Lens ((^.))
import qualified Data.Vector as V
import Linear.V2 (V2(..), _x, _y)
import Particles.Types

mapCellSize = 50 :: Float

makeParticlesMap :: BoundingBox -> Particles -> ParticlesMap
makeParticlesMap bbox particles =
    ParticlesMap { buckets = sortedParticles
                 , boundingBox = bbox
                 , width = mapWidth
                 , height = mapHeight
                 , cellSize = mapCellSize }
    where
        sortedParticles =
            V.accum appendParticle emptyBuckets labeledParticles
        appendParticle ps p = p : ps
        emptyBuckets = V.replicate numberOfCells []
        labeledParticles = labelParticle <$> particles
        labelParticle p = (particleCellIndex bbox mapCellSize p, p)
        numberOfCells = mapWidth * mapHeight
        mapWidth = ceiling $ (bbox ^. right - bbox ^. left) / mapCellSize
        mapHeight = ceiling $ (bbox ^. top - bbox ^. bottom) / mapCellSize

particlesInsideCell :: ParticlesMap -> CellIndex -> Particles
particlesInsideCell ParticlesMap{..} = V.unsafeIndex buckets

particleCellIndex :: BoundingBox -> CellSize -> Particle -> CellIndex
particleCellIndex BoundingBox{..} cellSize Particle{..} =
    column + row * columns
    where
        column = floor $ (_position ^. _x - _left) / cellSize
        row = floor $ (_position ^. _y - _bottom) / cellSize
        columns = ceiling $ (_right - _left) / cellSize
