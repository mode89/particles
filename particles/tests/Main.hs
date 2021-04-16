module Main where

import MapSpec (mapSpec)
import ModelSpec (modelSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
    mapSpec
    modelSpec
