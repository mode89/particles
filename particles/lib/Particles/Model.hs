{-# LANGUAGE RecordWildCards #-}

module Particles.Model where

import Control.Lens ((&), (^.), (.~))
import Control.Monad.State (runState, state)
import qualified Data.List as L
import Linear.Metric (dot, norm)
import Linear.V2 (V2(..), _x, _y)
import Linear.Vector ((^*), scaled)
import Particles.Map
import Particles.Types
import System.Random (mkStdGen, randomR, StdGen)

maxInitialSpeed = 400 :: Float
maxParticlesNum = 10000 :: Int
particleRadius = 10.0 :: Float
tickInterval = 0.04 :: Double

initialParticles :: BoundingBox -> Particles
initialParticles bbox =
    take 500 . L.unfoldr (Just . randomParticle bbox) $ mkStdGen 0

randomParticle :: BoundingBox -> StdGen -> (Particle, StdGen)
randomParticle BoundingBox{..} = runState $ do
    x <- state $ randomR (_left, _right)
    y <- state $ randomR (_bottom, _top)
    vx <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    vy <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    return $ Particle { _position = V2 x y, _velocity = V2 vx vy }

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

integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position +
      particle ^. velocity * (realToFrac tickInterval) )

handleCollisions :: ParticlesMap -> Particle -> Particle
handleCollisions particlesMap particle =
    foldr handleCollision particle $
        neighbourParticles particlesMap particle

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
