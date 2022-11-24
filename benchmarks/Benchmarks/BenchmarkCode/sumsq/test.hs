module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "returns correct output given example input" $ do
            sumsq 4 `shouldBe` 30
            sumsq 9 `shouldBe` 285