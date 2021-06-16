{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

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
maxInitialSpeed = 400

{-# INLINE maxParticlesNum #-}
maxParticlesNum :: Int
maxParticlesNum = 10000

{-# INLINE particleRadius #-}
particleRadius :: Fractional a => a
particleRadius = 10.0

{-# INLINE tickInterval #-}
tickInterval :: Fractional a => a
tickInterval = 0.04

initialParticles :: BoundingBox -> Particles
initialParticles bbox = runST $ do
    genRef <- newSTRef $ mkStdGen 0
    replicateM 500 $ randomParticle bbox genRef

randomParticle :: BoundingBox -> STRef s StdGen -> ST s Particle
randomParticle BoundingBox{..} genRef = do
    x <- random (_left, _right)
    y <- random (_bottom, _top)
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
bounceOfWalls bbox = bounceLeft
                   . bounceRight
                   . bounceBottom
                   . bounceTop
    where
        bounceLeft p =
            if p ^. position ^. _x < (bbox ^. left + particleRadius)
            then p & (velocity . _x) .~ (abs $ p ^. velocity ^. _x)
            else p
        bounceRight p =
            if p ^. position ^. _x > (bbox ^. right - particleRadius)
            then p & (velocity . _x) .~ (- (abs $ p ^. velocity ^. _x))
            else p
        bounceBottom p =
            if p ^. position ^. _y < (bbox ^. bottom + particleRadius)
            then p & (velocity . _y) .~ (abs $ p ^. velocity ^. _y)
            else p
        bounceTop p =
            if p ^. position ^. _y > (bbox ^. top - particleRadius)
            then p & (velocity . _y) .~ (- (abs $ p ^. velocity ^. _y))
            else p

{-# INLINE clampToBoundingBox #-}
clampToBoundingBox :: BoundingBox -> Particle -> Particle
clampToBoundingBox bbox = clampLeft
                        . clampRight
                        . clampBottom
                        . clampTop
    where
        clampLeft p =
            if p ^. position ^. _x < (bbox ^. left + particleRadius)
            then p & (position . _x) .~ (bbox ^. left + particleRadius)
            else p
        clampRight p =
            if p ^. position ^. _x > (bbox ^. right - particleRadius)
            then p & (position . _x) .~ (bbox ^. right - particleRadius)
            else p
        clampBottom p =
            if p ^. position ^. _y < (bbox ^. bottom + particleRadius)
            then p & (position . _y) .~ (bbox ^. bottom + particleRadius)
            else p
        clampTop p =
            if p ^. position ^. _y > (bbox ^. top - particleRadius)
            then p & (position . _y) .~ (bbox ^. top - particleRadius)
            else p
