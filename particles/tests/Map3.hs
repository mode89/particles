{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Map3 where

import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Linear.V2 (V2(..))
import Particles.Map3
import Particles.Map3.Types
import Particles.Types hiding (ParticlesMap)
import Test.Hspec

map3Spec :: Spec
map3Spec = describe "Map3" $ do

    it "encodeMorton16" $ do
        encodeMorton16 0b0000 0b0000 `shouldBe` 0b00000000
        encodeMorton16 0b0011 0b0011 `shouldBe` 0b00001111
        encodeMorton16 0b0111 0b0101 `shouldBe` 0b00110111
        encodeMorton16 0b0010 0b0110 `shouldBe` 0b00101100

    it "nextHighestPowerOf2" $ do
        nextHighestPowerOf2 3 `shouldBe` 4
        nextHighestPowerOf2 6 `shouldBe` 8
        nextHighestPowerOf2 11 `shouldBe` 16
        nextHighestPowerOf2 16 `shouldBe` 16
        nextHighestPowerOf2 17 `shouldBe` 32
        nextHighestPowerOf2 256 `shouldBe` 256

    it "getMapSize" $ do
        let bbox1 = makeBoundingBox 0 100 0 50
        getMapSize bbox1 30 `shouldBe` 4
        getMapSize bbox1 50 `shouldBe` 2
        getMapSize bbox1 70 `shouldBe` 2
        let bbox2 = makeBoundingBox 0 100 0 100
        getMapSize bbox2 30 `shouldBe` 4
        getMapSize bbox2 50 `shouldBe` 2
        getMapSize bbox2 70 `shouldBe` 2
        let bbox3 = makeBoundingBox 0 50 0 100
        getMapSize bbox3 30 `shouldBe` 4
        getMapSize bbox3 50 `shouldBe` 2
        getMapSize bbox3 70 `shouldBe` 2
        let bbox4 = makeBoundingBox 0 1920 0 1080
        getMapSize bbox4 50 `shouldBe` 64
        getMapSize bbox4 20 `shouldBe` 128

    it "bucketCoord" $ do
        let bbox = makeBoundingBox 0 70 0 70
        let pos = V2 60 25
        bucketCoord bbox 35 pos `shouldBe` (0, 1)

    it "make empty" $ do
        let bbox = makeBoundingBox 0 70 0 70
        let particles = VU.empty
        let pmap@ParticlesMap{..} = make 100 50 bbox particles
        let buckets = listFromMap 100 pmap
        mapSize `shouldBe` 2
        buckets `shouldBe` [[], [], [], []]

    it "make with one particle" $ do
        let bbox = makeBoundingBox 0 50 0 50
        let particles = VU.fromList [ Particle (V2 25 25) (V2 0 0) ]
        let pmap = make 100 50 bbox particles
        let buckets = listFromMap 100 pmap
        buckets `shouldBe` [[0]]

    it "make with two particles" $ do
        let bbox = makeBoundingBox 0 70 0 70
        let particles = VU.fromList
                [ Particle (V2 60 25) (V2 0 0)
                , Particle (V2 25 60) (V2 0 0) ]
        let pmap = make 100 50 bbox particles
        let buckets = listFromMap 100 pmap
        buckets `shouldBe` [[], [0], [1], []]

    it "neighbourBuckets" $ do
        Set.fromList (VU.toList $ neighbourBuckets 4 (1, 1)) `shouldBe`
            Set.fromList [0, 1, 2, 3, 4, 6, 8, 9, 12]
        Set.fromList (VU.toList $ neighbourBuckets 4 (1, 0)) `shouldBe`
            Set.fromList [0, 1, 2, 3, 8, 9]
        Set.fromList (VU.toList $ neighbourBuckets 4 (2, 3)) `shouldBe`
            Set.fromList [6, 7, 12, 13, 14, 15]

    it "neighbourParticles" $ do
        let bbox = makeBoundingBox 0 150 0 150
        let particles = VU.fromList
                [ Particle (V2 110 110) (V2 0 0)
                , Particle (V2  20  20) (V2 0 0)
                , Particle (V2  10  10) (V2 0 0)
                , Particle (V2  70  20) (V2 0 0)
                , Particle (V2  80  30) (V2 0 0)
                , Particle (V2  25  70) (V2 0 0) ]
        let pmap = make 100 50 bbox particles
        let neighbourParticles_ = neighbourParticles pmap (0, 0)
        Set.fromList (VU.toList neighbourParticles_) `shouldBe`
            Set.fromList [1, 2, 3, 4, 5]

listFromMap :: Int -> ParticlesMap -> [[Int]]
listFromMap bucketCapacity ParticlesMap{..} =
    zipWith listFromBucket [0..] bucketsSizes
    where
        listFromBucket index size = VU.toList $
            VU.slice (index * bucketCapacity) size mapBucketsStorage
        bucketsSizes = VU.toList mapBucketsSizes
