{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Model where

import Control.Lens ((&), (^.), (.~))
import Control.Monad (replicateM)
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, STRef, writeSTRef)
import Linear.Metric (dot, norm)
import Linear.V2 (V2(..), _x, _y)
import Linear.Vector ((^*))
import Particles.Map
import Particles.Types
import System.Random (mkStdGen, randomR, StdGen)

{-# INLINE maxInitialSpeed #-}
maxInitialSpeed :: Double
maxInitialSpeed = 100

{-# INLINE maxParticlesNum #-}
maxParticlesNum :: Int
maxParticlesNum = 10000

{-# INLINE particleRadius #-}
particleRadius :: Fractional a => a
particleRadius = 10.0

{-# INLINE tickInterval #-}
tickInterval :: Fractional a => a
tickInterval = 0.04

initialParticles :: Int -> BoundingBox -> Particles
initialParticles psNum bbox = runST $ do
    genRef <- newSTRef $ mkStdGen 0
    replicateM psNum $ randomParticle bbox genRef

randomParticle :: BoundingBox -> STRef s StdGen -> ST s Particle
randomParticle BoundingBox{..} genRef = do
    x <- random (bboxLeft, bboxRight)
    y <- random (bboxBottom, bboxTop)
    vx <- random (-maxInitialSpeed, maxInitialSpeed)
    vy <- random (-maxInitialSpeed, maxInitialSpeed)
    return $ Particle { _position = V2 x y, _velocity = V2 vx vy }
    where
        random range = do
            gen <- readSTRef genRef
            let (v, nextGen) = randomR range gen
            writeSTRef genRef nextGen
            return v

updateParticles :: Particles -> BoundingBox -> Particles
updateParticles particles bbox =
    updateParticle bbox particlesMap <$> particles
    where
        particlesMap = makeParticlesMap bbox particles

updateParticle :: BoundingBox -> ParticlesMap -> Particle -> Particle
updateParticle bbox particlesMap particle =
    clampToBoundingBox bbox .
    bounceOfWalls bbox .
    integrateVelocity .
    handleCollisions particlesMap $
        particle

{-# INLINE integrateVelocity #-}
integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position + particle ^. velocity * tickInterval )

handleCollisions :: ParticlesMap -> Particle -> Particle
handleCollisions particlesMap particle =
    foldr handleCollision particle $
        neighbourParticles particlesMap particle

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

{-# INLINE bounceOfWalls #-}
bounceOfWalls :: BoundingBox -> Particle -> Particle
bounceOfWalls BoundingBox{..} = bounceLeft
                              . bounceRight
                              . bounceBottom
                              . bounceTop
    where
        bounceLeft p =
            if p ^. position ^. _x < (bboxLeft + particleRadius)
            then p & (velocity . _x) .~ (abs $ p ^. velocity ^. _x)
            else p
        bounceRight p =
            if p ^. position ^. _x > (bboxRight - particleRadius)
            then p & (velocity . _x) .~ (- (abs $ p ^. velocity ^. _x))
            else p
        bounceBottom p =
            if p ^. position ^. _y < (bboxBottom + particleRadius)
            then p & (velocity . _y) .~ (abs $ p ^. velocity ^. _y)
            else p
        bounceTop p =
            if p ^. position ^. _y > (bboxTop - particleRadius)
            then p & (velocity . _y) .~ (- (abs $ p ^. velocity ^. _y))
            else p

{-# INLINE clampToBoundingBox #-}
clampToBoundingBox :: BoundingBox -> Particle -> Particle
clampToBoundingBox BoundingBox{..} = clampLeft
                                   . clampRight
                                   . clampBottom
                                   . clampTop
    where
        clampLeft p =
            if p ^. position ^. _x < (bboxLeft + particleRadius)
            then p & (position . _x) .~ (bboxLeft + particleRadius)
            else p
        clampRight p =
            if p ^. position ^. _x > (bboxRight - particleRadius)
            then p & (position . _x) .~ (bboxRight - particleRadius)
            else p
        clampBottom p =
            if p ^. position ^. _y < (bboxBottom + particleRadius)
            then p & (position . _y) .~ (bboxBottom + particleRadius)
            else p
        clampTop p =
            if p ^. position ^. _y > (bboxTop - particleRadius)
            then p & (position . _y) .~ (bboxTop - particleRadius)
            else p
