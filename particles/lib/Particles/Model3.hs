{-# LANGUAGE RecordWildCards #-}

module Particles.Model3 where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Particles.Map3 as Map3
import Particles.Map3.Types as Map3
import Particles.Types

data ModelState = ModelState
    { particles :: Particles2
    , tempParticles :: Particles2
    , particlesMap :: Map3.ParticlesMap }

unsafeUpdateState
    :: BucketCapacity
    -> CellSize
    -> BoundingBox
    -> ModelState
    -> ModelState
unsafeUpdateState bCapacity cSize bbox ModelState{..} =
    ModelState
        { particles = unsafeUpdateParticles
            bbox newParticlesMap particles tempParticles
        , tempParticles = particles
        , particlesMap = newParticlesMap }
    where
        newParticlesMap = Map3.unsafeUpdate
            bCapacity cSize bbox particles particlesMap

unsafeUpdateParticles
    :: BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> Particles2
    -> Particles2
unsafeUpdateParticles bbox pmap ps psDest = runST $ do
    psDestM <- VU.unsafeThaw psDest
    VU.ifoldM_ (\ pDestOffset bIndex bSize -> do
            let bBeginning = bIndex * Map3.mapBucketCapacity pmap
            let bParticleIndices = VU.unsafeSlice
                    bBeginning bSize (Map3.mapBucketsStorage pmap)
            VU.iforM_ bParticleIndices $ \ pDestI pSrcI -> do
                let p = VU.unsafeIndex ps pSrcI
                let pUpd = updateParticle bbox pmap ps p
                VUM.unsafeWrite psDestM (pDestOffset + pDestI) pUpd
            return $ pDestOffset + bSize
        ) 0 (Map3.mapBucketsSizes pmap)
    VU.unsafeFreeze psDestM

updateParticle
    :: BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> Particle
    -> Particle
updateParticle bbox pmap ps p = undefined
