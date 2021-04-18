{-# LANGUAGE TemplateHaskell #-}

module Particles.Types where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Control.Lens (makeLenses)
import qualified Data.Vector as V
import Linear.V2 (V2)

type Position = V2 Float

data Particle = Particle
    { _position :: Position
    , _velocity :: V2 Float } deriving (Eq, Show)
makeLenses ''Particle

instance NFData Particle where rnf = rwhnf

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
    , bucketDim :: BucketDim } deriving (Eq, Show)

type BucketIndex = Int
type BucketDim = Float
