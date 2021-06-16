{-# LANGUAGE RecordWildCards #-}

module Map2 where

import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Linear.V2 (V2(..))
import Particles.Map2
import Particles.Types
import Test.Hspec

map2Spec :: Spec
map2Spec = describe "Map2" $ do
    it "make empty" $ do
        let bbox = BoundingBox 0 70 0 70
        let particles = VU.empty
        let pmap@ParticlesMap2{..} = make bbox particles
        let buckets = listFromMap pmap
        mapWidth `shouldBe` 2
        mapHeight `shouldBe` 2
        buckets `shouldBe` [[], [], [], []]
    it "make with one particle" $ do
        let bbox = BoundingBox 0 50 0 50
        let particles = VU.fromList [ Particle (V2 25 25) (V2 0 0) ]
        let pmap = make bbox particles
        let buckets = listFromMap pmap
        buckets `shouldBe` [[0]]
    it "make with two particles" $ do
        let bbox = BoundingBox 0 70 0 70
        let particles = VU.fromList
                [ Particle (V2 60 60) (V2 0 0)
                , Particle (V2 25 25) (V2 0 0) ]
        let pmap = make bbox particles
        let buckets = listFromMap pmap
        buckets `shouldBe` [[1], [], [], [0]]
    it "neighbour buckets" $ do
        Set.fromList (VU.toList $ neighbourBuckets 3 3 1 1) `shouldBe`
            Set.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8]
        Set.fromList (VU.toList $ neighbourBuckets 3 3 1 0) `shouldBe`
            Set.fromList [0, 1, 3, 4, 6, 7]
        Set.fromList (VU.toList $ neighbourBuckets 3 3 2 2) `shouldBe`
            Set.fromList [4, 5, 7, 8]
    it "neighbour particles" $ do
        let bbox = BoundingBox 0 150 0 150
        let particles = VU.fromList
                [ Particle (V2 110 110) (V2 0 0)
                , Particle (V2  20  20) (V2 0 0)
                , Particle (V2  10  10) (V2 0 0)
                , Particle (V2  70  20) (V2 0 0)
                , Particle (V2  80  30) (V2 0 0)
                , Particle (V2  25  70) (V2 0 0) ]
        let pmap = make bbox particles
        let neighbourParticles_ = neighbourParticles pmap 0
        Set.fromList (VU.toList $ neighbourParticles_) `shouldBe`
            Set.fromList [1, 2, 3, 4, 5]

listFromMap :: ParticlesMap2 -> [[Int]]
listFromMap ParticlesMap2{..} = zipWith listFromBucket [0..] bucketsSizes
    where
        listFromBucket index size = VU.toList $
            VU.slice (index * maxBucketSize) size mapBucketsStorage
        bucketsSizes = VU.toList mapBucketsSizes
