{-# LANGUAGE RecordWildCards #-}

module Map2 where

import Control.Monad (forM_)
import Data.IORef (readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear.V2 (V2(..))
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
        mapBoundingBox `shouldBe` bbox
        mapBucketDim `shouldBe` 50
        buckets <- freezeBuckets mapBuckets
        buckets `shouldBe` [[], [], [], []]
    it "update, one particle" $ do
        let bbox = BoundingBox 0 50 0 50
        pmap0 <- initialize bbox
        let particles = VU.fromList [ Particle (V2 25 25) (V2 0 0) ]
        pmap1 <- update bbox particles pmap0
        buckets <- freezeBuckets $ mapBuckets pmap1
        buckets `shouldBe` [[0]]
    it "update, two particles" $ do
        let bbox = BoundingBox 0 70 0 70
        pmap0 <- initialize bbox
        let particles = VU.fromList
                [ Particle (V2 60 60) (V2 0 0)
                , Particle (V2 25 25) (V2 0 0) ]
        pmap1 <- update bbox particles pmap0
        buckets <- freezeBuckets $ mapBuckets pmap1
        buckets `shouldBe` [[1], [], [], [0]]

freezeBuckets :: MapBuckets -> IO [[Int]]
freezeBuckets (MapBuckets buckets) =
    V.toList <$> V.mapM freezeBucket buckets

freezeBucket :: MapBucket -> IO [Int]
freezeBucket MapBucket{..} = do
    size <- readIORef bucketParticlesNum
    VUM.foldr (:) [] $ VUM.take size bucketParticlesIndices
