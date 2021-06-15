{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Particles.Model2 where

import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.STRef (newSTRef)
import qualified Data.Vector.Unboxed as VU
import qualified Particles.Map2 as Map2
import qualified Particles.Model as Model
import Particles.Types
import System.Random (mkStdGen)

initialParticles :: BoundingBox -> Particles2
initialParticles bbox = runST $ do
    genRef <- newSTRef $ mkStdGen 0
    VU.replicateM 500 $ Model.randomParticle bbox genRef

updateParticles :: BoundingBox -> Particles2 -> Particles2
updateParticles bbox ps
    = VU.imap updateParticle_ ps
    where
        updateParticle_ = updateParticle pmap ps
        pmap = Map2.make bbox ps

{-# INLINE updateParticle #-}
updateParticle :: ParticlesMap2
               -> Particles2
               -> ParticleIndex
               -> Particle
               -> Particle
updateParticle pmap@ParticlesMap2{..} ps pIndex p
    = Model.clampToBoundingBox mapBoundingBox
    . Model.bounceOfWalls mapBoundingBox
    . Model.integrateVelocity
    $ handleCollisions pmap ps pIndex p

{-# INLINE handleCollisions #-}
handleCollisions :: ParticlesMap2
                 -> Particles2
                 -> ParticleIndex
                 -> Particle
                 -> Particle
handleCollisions pmap@ParticlesMap2{..} ps pIndex particle
    = VU.foldl handleCollision_ particle
    . VU.filter (/= pIndex)
    $ Map2.neighbourParticles pmap pBucketIndex
    where
        handleCollision_ p1 p2Index =
            let p2 = ps VU.! p2Index
            in Model.handleCollision p2 p1
        pBucketIndex = Map2.bucketIndex mapBoundingBox
                                        mapBucketDim
                                        (particle ^. position)
