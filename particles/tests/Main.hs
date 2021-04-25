module Main where

import MapSpec (mapSpec)
import Map2 (map2Spec)
import ModelSpec (modelSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
    mapSpec
    map2Spec
    modelSpec
