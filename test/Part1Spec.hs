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

  describe "Problem 2" $ do
    it "myButLast [1,2,3,4] == 3" $ do
      problem2 [1, 2, 3, 4] `shouldBe` 3

    it "myButLast ['a'..'z'] == 'y'" $ do
      problem2 ['a'..'z'] `shouldBe` 'y'
