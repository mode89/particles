{-# LANGUAGE RecordWildCards #-}

module Particles.Model where

import Control.Lens ((&), (^.), (.~))
import Control.Monad.State (runState, state)
import qualified Data.List as L
import Linear.Metric (dot, norm)
import Linear.V2 (V2(..), _x, _y)
import Linear.Vector ((^*), scaled)
import Particles.Types
import System.Random (mkStdGen, randomR, StdGen)

maxInitialSpeed = 400 :: Float
maxParticlesNum = 10000 :: Int
particleRadius = 10.0 :: Float
tickInterval = 0.04 :: Double

initialParticles :: Particles
initialParticles =
    -- initialParticles2
    take 1000 . L.unfoldr (Just . randomParticle) $ mkStdGen 0

initialParticles2 :: Particles
initialParticles2 =
    [ Particle { _position = V2 100 100, _velocity = V2 200 0 }
    , Particle { _position = V2 200 110, _velocity = V2 0 0 }
    , Particle { _position = V2 200 90, _velocity = V2 0 0 } ]

randomParticle :: StdGen -> (Particle, StdGen)
randomParticle = runState $ do
    x <- state $ randomR (100.0, 200.0)
    y <- state $ randomR (100.0, 200.0)
    vx <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    vy <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    return $ Particle { _position = V2 x y, _velocity = V2 vx vy }

updateParticles :: Particles -> BoundingBox -> Particles
updateParticles particles bbox =
    updateParticle bbox particles <$> particles

updateParticle :: BoundingBox -> Particles -> Particle -> Particle
updateParticle bbox particles particle =
    clampToBoundingBox bbox .
    bounceOfWalls bbox .
    integrateVelocity .
    handleCollisions (filter (/= particle) particles) $
        particle

integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position +
      particle ^. velocity * (realToFrac tickInterval) )

handleCollisions :: Particles -> Particle -> Particle
handleCollisions particles particle =
    foldr handleCollision particle particles

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
