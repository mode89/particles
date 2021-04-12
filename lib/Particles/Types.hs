{-# LANGUAGE TemplateHaskell #-}

module Particles.Types where

import Control.Lens (makeLenses)
import qualified Data.Vector as V
import Linear.V2 (V2)

data Particle = Particle
    { _position :: V2 Float
    , _velocity :: V2 Float } deriving (Eq, Show)
makeLenses ''Particle

type Particles = [Particle]

data BoundingBox = BoundingBox
    { _left :: Float
    , _right :: Float
    , _bottom :: Float
    , _top :: Float } deriving (Eq, Show)
makeLenses ''BoundingBox

data ParticlesMap = ParticlesMap
    { buckets :: V.Vector Particles
    , boundingBox :: BoundingBox
    , width :: Int
    , height :: Int
    , cellSize :: CellSize } deriving (Eq, Show)

type CellIndex = Int
type CellSize = Float
