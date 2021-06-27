{-# LANGUAGE BangPatterns #-}

module Particles.Map3.Types where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Particles.Types hiding (ParticlesMap)

data ParticlesMap = ParticlesMap
    { mapBucketsSizes :: MapBucketsSizes
    , mapBucketsStorage :: MapBucketsStorage
    , mapBucketCapacity :: !Int
    , mapSize :: !Int }

instance NFData ParticlesMap where rnf = rwhnf

data MParticlesMap = MParticlesMap
    { mapBucketsSizesM :: MMapBucketsSizes
    , mapBucketsStorageM :: MMapBucketsStorage
    , mapBucketCapacityM :: !Int
    , mapSizeM :: !Int }

instance NFData MParticlesMap where rnf = rwhnf
