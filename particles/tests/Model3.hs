{-# LANGUAGE RecordWildCards #-}

module Model3 where

import Control.Lens ((^.))
import qualified Data.Vector.Unboxed as VU
import Linear.V2 (V2(..))
import Particles.Map3.Types as M3
import Particles.Map3 as M3
import Particles.Model3
import Particles.Types
import Test.Hspec
import Test.HUnit.Approx (assertApproxEqual)

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

    it "handleCollisions, single particle" $ do
        let bbox = makeBoundingBox 0 50 0 50
        let p = Particle (V2 25 25) (V2 10 10)
        let ps = VU.fromList [p]
        let pmap = M3.make 10 50 bbox ps
        let pUpd = handleCollisions 50 bbox pmap ps 0 p
        pUpd `shouldBe` p

    it "handleCollisions, two distant particles" $ do
        let bbox = makeBoundingBox 0 50 0 50
        let p0 = Particle (V2 15 15) (V2 10 10)
        let p1 = Particle (V2 35 35) (V2 10 10)
        let ps = VU.fromList [p0, p1]
        let pmap = M3.make 10 50 bbox ps
        let p0' = handleCollisions 50 bbox pmap ps 0 p0
        let p1' = handleCollisions 50 bbox pmap ps 1 p1
        p0' `shouldBe` p0
        p1' `shouldBe` p1

    it "handleCollisions, two close particles" $ do
        let bbox = makeBoundingBox 0 50 0 50
        let p0 = Particle (V2 20 20) (V2 6 8)
        let p1 = Particle (V2 26 28) (V2 (-12) (-16))
        let ps = VU.fromList [p0, p1]
        let pmap = M3.make 10 50 bbox ps
        let p0' = handleCollisions 50 bbox pmap ps 0 p0
        let p1' = handleCollisions 50 bbox pmap ps 1 p1
        let repulsion = V2 3 4
        p0' `shouldBeParticle` Particle (p0 ^. position - repulsion) (p1 ^. velocity)
        p1' `shouldBeParticle` Particle (p1 ^. position + repulsion) (p0 ^. velocity)

    it "repulsion" $ do
        let p1 = V2 0 0
        let p2 = V2 0.8 0.6
        repulse 0.8 p1 p2 `shouldBeV2` V2 (-0.24) (-0.18)
        return () :: IO ()

shouldBeParticle :: Particle -> Particle -> Expectation
shouldBeParticle
    (Particle (V2 pxA pyA) (V2 vxA vyA))
    (Particle (V2 pxE pyE) (V2 vxE vyE)) = do
    assertApproxEqual "Position X" epsilon pxE pxA
    assertApproxEqual "Position Y" epsilon pyE pyA
    assertApproxEqual "Velocity X" epsilon vxE vxA
    assertApproxEqual "Velocity Y" epsilon vyE vyA
    where
        epsilon = 1e-6

shouldBeV2 :: V2 Double -> V2 Double -> Expectation
shouldBeV2 (V2 xA yA) (V2 xE yE) = do
    assertApproxEqual "X" epsilon xE xA
    assertApproxEqual "Y" epsilon yE yA
    where
        epsilon = 1e-6
