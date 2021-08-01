{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Particles.Model3 where

import Control.DeepSeq (NFData)
import Control.Lens ((&), (^.), (.~))
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace
import GHC.Generics (Generic)
import Linear.Metric (dot, norm)
import Linear.V2 (V2(..), _x, _y)
import Linear.Vector ((^*))
import Particles.Map3 as Map3
import Particles.Map3.Types as Map3
import qualified Particles.Model as Model
import qualified Particles.Model2 as Model2
import Particles.Types
import System.Random (mkStdGen, randomR, StdGen)

data ModelState = ModelState
    { particles :: Particles2
    , tempParticles :: Particles2
    , particlesMap :: Map3.ParticlesMap }
    deriving (Generic, NFData, Show)

initialState
    :: BucketCapacity
    -> CellSize
    -> BoundingBox
    -> Int
    -> ModelState
initialState bCapacity cSize bbox psNum =
    ModelState
        { particles = ps
        , tempParticles = VU.replicate psNum $ Particle (V2 0 0) (V2 0 0)
        , particlesMap = Map3.make bCapacity cSize bbox ps }
    where
        ps = Model2.initialParticles psNum bbox

unsafeUpdateState
    :: BucketCapacity
    -> CellSize
    -> BoundingBox
    -> ModelState
    -> ModelState
unsafeUpdateState bCapacity cSize bbox ModelState{..} =
    ModelState
        { particles = updatedParticles
        , tempParticles = oldParticles
        , particlesMap = updatedMap }
    where
        oldParticles = particles
        updatedMap = Map3.unsafeUpdate
            bCapacity cSize bbox particles particlesMap
        !updatedParticles = unsafeUpdateParticles
            cSize bbox updatedMap particles tempParticles

{-# INLINE unsafeUpdateParticles #-}
unsafeUpdateParticles
    :: CellSize
    -> BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> Particles2
    -> Particles2
unsafeUpdateParticles !cSize bbox pmap ps psDest = runST $ do
    psDestM <- VU.unsafeThaw psDest
    VU.ifoldM_ (\ pDestOffset bIndex bSize -> do
            let bBeginning = bIndex * Map3.mapBucketCapacity pmap
            let bParticleIndices = VU.slice
                    bBeginning bSize (Map3.mapBucketsStorage pmap)
            VU.iforM_ bParticleIndices $ \ pDestI pSrcI -> do
                let p = ps VU.! pSrcI
                let pUpd = updateParticle cSize bbox pmap ps pSrcI p
                VUM.write psDestM (pDestOffset + pDestI) pUpd
            return $ pDestOffset + bSize
        ) 0 (Map3.mapBucketsSizes pmap)
    VU.unsafeFreeze psDestM

{-# INLINE updateParticle #-}
updateParticle
    :: CellSize
    -> BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> ParticleIndex
    -> Particle
    -> Particle
updateParticle cSize bbox pmap ps pIndex
    = clampToBoundingBox bbox
    . bounceOfWalls bbox
    . integrateVelocity
    . handleCollisions cSize bbox pmap ps pIndex

{-# INLINE clampToBoundingBox #-}
clampToBoundingBox :: BoundingBox -> Particle -> Particle
clampToBoundingBox BoundingBox{..} = clampLeft
                                   . clampRight
                                   . clampBottom
                                   . clampTop
    where
        clampLeft p =
            if p ^. (position . _x) < (bboxLeft + Model.particleRadius)
            then p & (position . _x) .~ (bboxLeft + Model.particleRadius)
            else p
        clampRight p =
            if p ^. (position . _x) > (bboxRight - Model.particleRadius)
            then p & (position . _x) .~ (bboxRight - Model.particleRadius)
            else p
        clampBottom p =
            if p ^. (position . _y) < (bboxBottom + Model.particleRadius)
            then p & (position . _y) .~ (bboxBottom + Model.particleRadius)
            else p
        clampTop p =
            if p ^. (position . _y) > (bboxTop - Model.particleRadius)
            then p & (position . _y) .~ (bboxTop - Model.particleRadius)
            else p

{-# INLINE bounceOfWalls #-}
bounceOfWalls :: BoundingBox -> Particle -> Particle
bounceOfWalls BoundingBox{..} = bounceLeft
                              . bounceRight
                              . bounceBottom
                              . bounceTop
    where
        bounceLeft p =
            if p ^. (position . _x) < (bboxLeft + Model.particleRadius)
            then p & (velocity . _x) .~ abs (p ^. (velocity . _x))
            else p
        bounceRight p =
            if p ^. (position . _x) > (bboxRight - Model.particleRadius)
            then p & (velocity . _x) .~ (- (abs $ p ^. (velocity . _x)))
            else p
        bounceBottom p =
            if p ^. (position . _y) < (bboxBottom + Model.particleRadius)
            then p & (velocity . _y) .~ abs (p ^. (velocity . _y))
            else p
        bounceTop p =
            if p ^. (position . _y) > (bboxTop - Model.particleRadius)
            then p & (velocity . _y) .~ (- (abs $ p ^. (velocity . _y)))
            else p

{-# INLINE integrateVelocity #-}
integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position + particle ^. velocity * Model.tickInterval )

{-# INLINE handleCollisions #-}
handleCollisions
    :: CellSize
    -> BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> ParticleIndex
    -> Particle
    -> Particle
handleCollisions cSize bbox pmap ps pIndex particle
    = VU.foldl' handleCollision_ particle
    . VU.filter (/= pIndex)
    $ Map3.neighbourParticles pmap bCoord
    where
        handleCollision_ p1 p2Index =
            let p2 = ps VU.! p2Index
            in handleCollision p2 p1
        bCoord = Map3.bucketCoord bbox cSize (particle ^. position)

{-# INLINE handleCollision #-}
handleCollision :: Particle -> Particle -> Particle
handleCollision anotherParticle particleOfInterest =
    if onCollistionDistance
    then particleOfInterest
        & velocity .~ (v1 - dp ^* (dot_dv_dp / (norm_dp ** 2)))
        & position .~ repulse Model.particleRadius p1 p2
    else particleOfInterest
    where
        collision = onCollistionDistance
        onCollistionDistance = norm_dp < 2 * Model.particleRadius
        movingTowardsEachOther = dot_dv_dp < 0
        norm_dp = norm dp
        dot_dv_dp = dot dv dp
        dv = v1 - v2
        dp = p1 - p2
        v1 = particleOfInterest ^. velocity
        p1 = particleOfInterest ^. position
        v2 = anotherParticle ^. velocity
        p2 = anotherParticle ^. position

{-# INLINE repulse #-}
repulse :: Double -> Position -> Position -> Position
repulse pRadius p1 p2 = p1 + delta where
    delta = distV ^* (overlap * 0.5 / dist)
    distV = p1 - p2
    overlap = 2 * pRadius - dist
    dist = norm distV
