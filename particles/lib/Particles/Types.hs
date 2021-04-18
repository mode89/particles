{-# LANGUAGE TemplateHaskell #-}

module Particles.Types where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Control.Lens (makeLenses)
import qualified Data.Vector as V
import Linear.V2 (V2)

type Position = V2 Double

data Particle = Particle
    { _position :: Position
    , _velocity :: V2 Double } deriving (Eq, Show)
makeLenses ''Particle

instance NFData Particle where rnf = rwhnf

type Particles = [Particle]

data BoundingBox = BoundingBox
    { _left :: Double
    , _right :: Double
    , _bottom :: Double
    , _top :: Double } deriving (Eq, Show)
makeLenses ''BoundingBox

data ParticlesMap = ParticlesMap
    { buckets :: V.Vector Particles
    , boundingBox :: BoundingBox
    , width :: Int
    , bucketDim :: BucketDim } deriving (Eq, Show)

type BucketIndex = Int
type BucketDim = Double
