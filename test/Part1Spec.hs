module Part1Spec (spec) where

import Test.Hspec
import Part1

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "myLast [1,2,3,4] == 4" $ do
      problem1 [1, 2, 3, 4] `shouldBe` 4

    it "myLast ['x','y','z'] == 'z'" $ do
      problem1 ['x', 'y', 'z'] `shouldBe` 'z'
