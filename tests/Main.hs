module Main where

import ModelSpec (modelSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
    modelSpec
