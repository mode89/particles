{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Model2 where

import Control.Lens ((&), (^.), (.~))
import Control.Monad.ST (runST)
import Data.STRef (newSTRef)
import qualified Data.Vector.Unboxed as VU
import Linear.Metric (dot, norm)
import Linear.V2 (_x, _y)
import Linear.Vector ((^*))
import qualified Particles.Map2 as Map2
import qualified Particles.Model as Model
import Particles.Model (particleRadius, tickInterval)
import Particles.Types
import System.Random (mkStdGen)

initialParticles :: Int -> BoundingBox -> Particles2
initialParticles psNum bbox = runST $ do
    genRef <- newSTRef $ mkStdGen 0
    VU.replicateM psNum $ Model.randomParticle bbox genRef

updateParticles :: BucketCapacity
                -> CellSize
                -> BoundingBox
                -> Particles2
                -> Particles2
updateParticles !bCapacity !cSize !bbox ps
    = VU.imap updateParticle_ ps
    where
        updateParticle_ = updateParticle pmap ps bbox cSize
        !pmap = Map2.make bCapacity cSize bbox ps

{-# INLINE updateParticle #-}
updateParticle :: ParticlesMap2
               -> Particles2
               -> BoundingBox
               -> CellSize
               -> ParticleIndex
               -> Particle
               -> Particle
updateParticle pmap ps bbox cSize pIndex p
    = clampToBoundingBox bbox
    . bounceOfWalls bbox
    . integrateVelocity
    $ handleCollisions pmap ps bbox cSize pIndex p

{-# INLINE clampToBoundingBox #-}
clampToBoundingBox :: BoundingBox -> Particle -> Particle
clampToBoundingBox BoundingBox{..} = clampLeft
                                   . clampRight
                                   . clampBottom
                                   . clampTop
    where
        clampLeft p =
            if p ^. (position . _x) < (bboxLeft + particleRadius)
            then p & (position . _x) .~ (bboxLeft + particleRadius)
            else p
        clampRight p =
            if p ^. (position . _x) > (bboxRight - particleRadius)
            then p & (position . _x) .~ (bboxRight - particleRadius)
            else p
        clampBottom p =
            if p ^. (position . _y) < (bboxBottom + particleRadius)
            then p & (position . _y) .~ (bboxBottom + particleRadius)
            else p
        clampTop p =
            if p ^. (position . _y) > (bboxTop - particleRadius)
            then p & (position . _y) .~ (bboxTop - particleRadius)
            else p

{-# INLINE bounceOfWalls #-}
bounceOfWalls :: BoundingBox -> Particle -> Particle
bounceOfWalls BoundingBox{..} = bounceLeft
                              . bounceRight
                              . bounceBottom
                              . bounceTop
    where
        bounceLeft p =
            if p ^. (position . _x) < (bboxLeft + particleRadius)
            then p & (velocity . _x) .~ abs (p ^. (velocity . _x))
            else p
        bounceRight p =
            if p ^. (position . _x) > (bboxRight - particleRadius)
            then p & (velocity . _x) .~ (- (abs $ p ^. (velocity . _x)))
            else p
        bounceBottom p =
            if p ^. (position . _y) < (bboxBottom + particleRadius)
            then p & (velocity . _y) .~ abs (p ^. (velocity . _y))
            else p
        bounceTop p =
            if p ^. (position . _y) > (bboxTop - particleRadius)
            then p & (velocity . _y) .~ (- (abs $ p ^. (velocity . _y)))
            else p

{-# INLINE integrateVelocity #-}
integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position + particle ^. velocity * tickInterval )

{-# INLINE handleCollisions #-}
handleCollisions :: ParticlesMap2
                 -> Particles2
                 -> BoundingBox
                 -> CellSize
                 -> ParticleIndex
                 -> Particle
                 -> Particle
handleCollisions pmap ps bbox cSize pIndex particle
    = VU.foldl handleCollision_ particle
    . VU.filter (/= pIndex)
    $ Map2.neighbourParticles pmap pBucketIndex
    where
        handleCollision_ p1 p2Index =
            let p2 = ps VU.! p2Index
            in handleCollision p2 p1
        pBucketIndex = Map2.bucketIndex bbox cSize (particle ^. position)

{-# INLINE handleCollision #-}
handleCollision :: Particle -> Particle -> Particle
handleCollision anotherParticle particleOfInterest =
    if collision
    then particleOfInterest
        & velocity .~ (v1 - dp ^* (dot_dv_dp / (norm_dp ** 2)))
    else particleOfInterest
    where
        collision = onCollistionDistance && movingTowardsEachOther
        onCollistionDistance = norm_dp < 2 * particleRadius
        movingTowardsEachOther = dot_dv_dp < 0
        norm_dp = norm dp
        dot_dv_dp = dot dv dp
        dv = v1 - v2
        dp = p1 - p2
        v1 = particleOfInterest ^. velocity
        p1 = particleOfInterest ^. position
        v2 = anotherParticle ^. velocity
        p2 = anotherParticle ^. position

-- Tests for optimizing core

-- testHC :: Particle -> Particle -> Double
-- testHC p1 p2 = norm $ pos + vel where
--     pos = p ^. position
--     vel = p ^. velocity
--     p = handleCollision p1 p2

-- testHCs :: ParticlesMap2
--         -> Particles2
--         -> BoundingBox
--         -> CellSize
--         -> ParticleIndex
--         -> Double

-- testHCs pmap ps bbox cSize pIndex =
--     let p = ps VU.! pIndex
--         pr = handleCollisions pmap ps bbox cSize pIndex p
--         pos = pr ^. position
--         vel = pr ^. velocity
--     in norm $ pos + vel
