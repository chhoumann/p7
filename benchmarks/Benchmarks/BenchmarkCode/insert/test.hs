module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main = hspec $ do
    describe "Session6.insert" $ do
            it "returns a search tree after insertion in a search tree" $do
                let st = Node (Leaf 1) 2 (Leaf 3)
                let ft = Node (Node (Leaf 1) 2 Empty) 2 (Leaf 3)

                -- Just making sure
                flatten st `shouldBe` [1,2,3]
                flatten ft `shouldBe` [1,2,2,3]

                flatten (insert st 2) `shouldBe` flatten ft

                flatten (insert (insert (insert (insert Empty 1) 2) 18) 13) `shouldBe` [1, 2, 13, 18]