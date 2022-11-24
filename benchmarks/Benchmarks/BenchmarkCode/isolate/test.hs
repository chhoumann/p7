module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "returns correct output given example input" $ do
            isolate [4,5,4,6,7,4] 4 `shouldBe` ([5,6,7],[4,4,4])
            isolate ['g','a','k','a'] 'a' `shouldBe` (['g','k'], ['a','a'])