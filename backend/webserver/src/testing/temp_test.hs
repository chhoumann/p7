import Test.Hspec
import Test.QuickCheck
import Code (add)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "should evaluate 2 + 2 = 4" $ do
      add 2 2 `shouldBe` (4 :: Int)
