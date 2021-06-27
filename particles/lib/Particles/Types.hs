{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Particles.Types where

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Generics (Generic)
import Linear.V2 (V2)

type Position = V2 Double
type Velocity = V2 Double

data Particle = Particle
    { _position :: !Position
    , _velocity :: !Velocity } deriving (Eq, Generic, NFData, Show)
makeLenses ''Particle

derivingUnbox "Particle"
    [t| Particle -> (V2 Double, V2 Double) |]
    [| \ (Particle p v) -> (p, v) |]
    [| \ (p, v) -> Particle p v |]

type Particles = [Particle]
type Particles2 = VU.Vector Particle

data BoundingBox = BoundingBox
    { bboxLeft :: !Double
    , bboxRight :: !Double
    , bboxBottom :: !Double
    , bboxTop :: !Double
    , bboxWidth :: !Double
    , bboxHeight :: !Double } deriving (Eq, Show)

makeBoundingBox :: Double -> Double -> Double -> Double -> BoundingBox
makeBoundingBox l r b t = BoundingBox l r b t w h where
    w = assert (r > l) (r - l)
    h = assert (t > b) (t - b)

data ParticlesMap = ParticlesMap
    { buckets :: V.Vector Particles
    , boundingBox :: BoundingBox
    , width :: Int
    , bucketDim :: BucketDim } deriving (Eq, Show)

data ParticlesMap2 = ParticlesMap2
    { mapBucketsSizes :: MapBucketsSizes
    , mapBucketsStorage :: MapBucketsStorage
    , mapBucketCapacity :: !Int
    , mapWidth :: !Int
    , mapHeight :: !Int }
    deriving (Generic, NFData)

data ParticlesState = ParticlesState
    { particles :: Particles2
    , particlesMap :: ParticlesMap2 }

type MapBucketsSizes = VU.Vector Int
type MapBucketsStorage = VU.Vector ParticleIndex
type MMapBucketsSizes = VUM.IOVector Int
type MMapBucketsStorage = VUM.IOVector ParticleIndex
type ParticleIndex = Int
type BucketIndex = Int
type BucketDim = Double
type BucketCoord = (Int, Int)
type BucketCapacity = Int
type BucketSize = Double
type MapSize = Int
