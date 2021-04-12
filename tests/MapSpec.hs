module MapSpec where

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
        particleCellIndex bbox 50 particle `shouldBe` 3
    it "make from one particle" $ do
        let bbox = BoundingBox 0 50 0 50
            particle = Particle
                { _position = V2 25 25
                , _velocity = V2 0 0 }
        makeParticlesMap bbox [particle] `shouldBe`
            ParticlesMap { buckets = V.singleton [particle]
                         , boundingBox = bbox
                         , width = 1
                         , height = 1
                         , cellSize = 50 }
    it "make from two particles" $ do
        let bbox = BoundingBox 0 100 0 100
            p1 = Particle { _position = V2 75 75 , _velocity = V2 0 0 }
            p2 = Particle { _position = V2 25 25 , _velocity = V2 0 0 }
        makeParticlesMap bbox [p1, p2] `shouldBe`
            ParticlesMap { buckets = V.fromList [[p2], [], [], [p1]]
                         , boundingBox = bbox
                         , width = 2
                         , height = 2
                         , cellSize = 50 }
