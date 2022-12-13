module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "returns correct output given example input" $ do
            Code.within [1,3,4,5,2] (1,3) `shouldBe` [1,3,2]
            Code.within [1,3,4,5,2] (3,1) `shouldBe` []