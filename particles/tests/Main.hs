module Main where

import MapSpec (mapSpec)
import Map2 (map2Spec)
import Map3 (map3Spec)
import ModelSpec (modelSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
    mapSpec
    map2Spec
    map3Spec
    modelSpec
