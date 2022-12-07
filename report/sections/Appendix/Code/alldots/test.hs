module SolutionSpec where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "returns dot products of two lists of tuples" $ do
            alldots [(1, 1), (2, 2)] [(1, 1), (2, 2)] `shouldBe` [2, 4, 4, 8]
        it "returns dot products of two lists of tuples" $ do
            alldots [(4, 1), (2, 7)] [(5, 3), (1, 4)] `shouldBe` [23, 8, 31, 30]