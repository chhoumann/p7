module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Session3.pythagoreanTriple" $ do
        it "returns [(3, 4, 5)] with k = 5" $ do
            pythagoreanTriple 5 `shouldBe` [(3, 4, 5)]
        it "returns a list containing correct pythagorean triples" $ do
            pythagoreanTriple 41 `shouldContain` [ (3, 4, 5) ]
            pythagoreanTriple 41 `shouldContain` [ (5, 12, 13) ]
            pythagoreanTriple 41 `shouldContain` [ (7, 24, 25) ]
            pythagoreanTriple 41 `shouldContain` [ (8, 15, 17) ]
            pythagoreanTriple 41 `shouldContain` [ (9, 40, 41) ]