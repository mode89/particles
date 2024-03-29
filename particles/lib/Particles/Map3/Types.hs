{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Particles.Map3.Types where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Particles.Types hiding (ParticlesMap)

data ParticlesMap = ParticlesMap
    { mapBucketsSizes :: MapBucketsSizes
    , mapBucketsStorage :: MapBucketsStorage
    , mapBucketCapacity :: !Int
    , mapSize :: !Int }
    deriving (Generic, NFData, Show)
