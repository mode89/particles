{-# LANGUAGE RecordWildCards #-}

module Particles.Model3 where

import Control.Monad.ST (runST)
import Control.Lens ((^.))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (V2(..))
import Particles.Map3 as Map3
import Particles.Map3.Types as Map3
import qualified Particles.Model as Model
import qualified Particles.Model2 as Model2
import Particles.Types

data ModelState = ModelState
    { particles :: Particles2
    , tempParticles :: Particles2
    , particlesMap :: Map3.ParticlesMap }

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
            let bParticleIndices = VU.slice
                    bBeginning bSize (Map3.mapBucketsStorage pmap)
            VU.iforM_ bParticleIndices $ \ pDestI pSrcI -> do
                let p = ps VU.! pSrcI
                let pUpd = updateParticle bbox pmap ps p
                VUM.write psDestM (pDestOffset + pDestI) pUpd
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

handleCollisions
    :: CellSize
    -> BoundingBox
    -> Map3.ParticlesMap
    -> Particles2
    -> ParticleIndex
    -> Particle
    -> Particle
handleCollisions cSize bbox pmap ps pIndex particle
    = VU.foldl handleCollision_ particle
    . VU.filter (/= pIndex)
    $ Map3.neighbourParticles pmap bCoord
    where
        handleCollision_ p1 p2Index =
            let p2 = ps VU.! p2Index
            in Model.handleCollision p2 p1
        bCoord = Map3.bucketCoord bbox cSize (particle ^. position)