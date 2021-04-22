{-# LANGUAGE DisambiguateRecordFields #-}

module MapSpec where

import Control.Lens ((^.))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear.V2 (V2(..))
import Particles.Map
import Particles.Types
import Test.Hspec

mapSpec :: Spec
mapSpec = describe "Map" $ do
    it "particle cell index" $ do
        let bbox = BoundingBox 0 100 0 100
            particle = Particle
                { _position = V2 60 60
                , _velocity = V2 0 0 }
        particleBucketIndex bbox 50 (particle ^. position) `shouldBe` 3
    it "make from one particle" $ do
        let bbox = BoundingBox 0 50 0 50
            particle = Particle
                { _position = V2 25 25
                , _velocity = V2 0 0 }
        makeParticlesMap bbox [particle] `shouldBe`
            ParticlesMap { buckets = V.singleton [particle]
                         , boundingBox = bbox
                         , width = 1
                         , bucketDim = 50 }
    it "make from two particles" $ do
        let bbox = BoundingBox 0 100 0 100
            p1 = Particle { _position = V2 75 75 , _velocity = V2 0 0 }
            p2 = Particle { _position = V2 25 25 , _velocity = V2 0 0 }
        makeParticlesMap bbox [p1, p2] `shouldBe`
            ParticlesMap { buckets = V.fromList [[p2], [], [], [p1]]
                         , boundingBox = bbox
                         , width = 2
                         , bucketDim = 50 }
    it "no neighbours" $ do
        let p = Particle { _position = V2 25 25
                         , _velocity = V2 0 0 }
            bbox = BoundingBox 0 50 0 50
            pmap = ParticlesMap { buckets = V.fromList [[p]]
                                , boundingBox = bbox
                                , width = 1
                                , bucketDim = 50 }
        neighbourParticles pmap p `shouldBe` []
    it "neighbour buckets" $ do
        let pmap = ParticlesMap { buckets = V.replicate 9 []
                                , boundingBox = BoundingBox 0 150 0 150
                                , width = 3
                                , bucketDim = 50 }
        Set.fromList (neighbourBuckets pmap 4) `shouldBe`
            Set.fromList [0, 1, 2, 3, 5, 6, 7, 8]
