{-# LANGUAGE RecordWildCards #-}

module Model3 where

import qualified Data.Vector.Unboxed as VU
import Particles.Map3.Types as M3
import Particles.Model3
import Particles.Types
import Test.Hspec

model3Spec :: Spec
model3Spec = describe "Model3" $ do

    it "initialState" $ do
        let bbox = makeBoundingBox 0 90 0 60
        let ModelState{..} = initialState 150 50 bbox 70
        let M3.ParticlesMap{..} = particlesMap
        VU.length particles `shouldBe` 70
        VU.length tempParticles `shouldBe` 70
        VU.length mapBucketsSizes `shouldBe` 4
        VU.length mapBucketsStorage `shouldBe` 4 * 150
        mapBucketCapacity `shouldBe` 150
        mapSize `shouldBe` 2
