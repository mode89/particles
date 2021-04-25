{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Particles.Types where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Control.Lens (makeLenses)
import Data.IORef (IORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear.V2 (V2)

type Position = V2 Double

data Particle = Particle
    { _position :: Position
    , _velocity :: V2 Double } deriving (Eq, Show)
makeLenses ''Particle

instance NFData Particle where rnf = rwhnf

derivingUnbox "Particle"
    [t| Particle -> (V2 Double, V2 Double) |]
    [| \ (Particle p v) -> (p, v) |]
    [| \ (p, v) -> Particle p v |]

type Particles = [Particle]
type Particles2 = VU.Vector Particle

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

data ParticlesMap2 = ParticlesMap2
    { mapBuckets :: MapBuckets
    , mapBoundingBox :: BoundingBox
    , mapBucketDim :: BucketDim }

data MapBuckets = MapBuckets (V.Vector MapBucket)

data MapBucket = MapBucket
    { bucketParticlesNum :: IORef Int
    , bucketParticlesIndices :: VUM.IOVector Int }

data ParticlesState = ParticlesState
    { particles :: Particles2
    , particlesMap :: ParticlesMap2 }

type BucketIndex = Int
type BucketDim = Double
