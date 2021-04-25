{-# LANGUAGE RecordWildCards #-}

module Map2 where

import Data.IORef (readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Particles.Map2
import Particles.Types
import Test.Hspec

map2Spec :: Spec
map2Spec = describe "Map2" $ do
    it "newBucket" $ do
        MapBucket{..} <- newBucket
        size <- readIORef $ bucketParticlesNum
        size `shouldBe` 0
        VUM.length bucketParticlesIndices `shouldBe` 100
    it "initialize" $ do
        let bbox = BoundingBox 0 70 0 70
        ParticlesMap2{..} <- initialize bbox
        let (MapBuckets buckets) = mapBuckets
        V.length buckets `shouldBe` 4
        mapBoundingBox `shouldBe` bbox
        mapBucketDim `shouldBe` 50
        V.forM_ buckets $ \MapBucket{..} -> do
            size <- readIORef $ bucketParticlesNum
            size `shouldBe` 0
            VUM.length bucketParticlesIndices `shouldBe` 100
