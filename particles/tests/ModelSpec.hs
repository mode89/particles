module ModelSpec where

import Control.Lens ((^.))
import Linear.V2 (V2(..))
import Particles.Model
import Particles.Types
import Test.Hspec

modelSpec :: Spec
modelSpec = describe "Model" $ do
    it "clamp to left wall" $ do
        let bbox = makeBoundingBox 0 100 0 100
            particle = Particle
                { _position = V2 (-1) 50
                , _velocity = V2 0 0 }
        clampToBoundingBox bbox particle `shouldBe`
            particle { _position = V2 10 50 }
