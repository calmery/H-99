module Part1Spec (spec) where

import Test.Hspec
import Part1

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "myLast [1, 2, 3, 4] == 4" $ do
      problem1 [1, 2, 3, 4] `shouldBe` 4

    it "myLast ['x', 'y', 'z'] == 'z'" $ do
      problem1 ['x', 'y', 'z'] `shouldBe` 'z'

  describe "Problem 2" $ do
    it "myButLast [1, 2, 3, 4] == 3" $ do
      problem2 [1, 2, 3, 4] `shouldBe` 3

    it "myButLast ['a'..'z'] == 'y'" $ do
      problem2 ['a'..'z'] `shouldBe` 'y'

  describe "Problem 3" $ do
    it "elementAt [1, 2, 3] 2 == 2" $ do
      problem3 [1, 2, 3] 2 `shouldBe` 2

    it "elementAt \"haskell\" 5 == 'e'" $ do
      problem3 "haskell" 5 `shouldBe` 'e'

  describe "Problem 4" $ do
    it "myLength [123, 456, 789] == 3" $ do
      problem4 [123, 456, 789] `shouldBe` 3

    it "myLength \"Hello, world!\" == 13" $ do
      problem4 "Hello, world!" `shouldBe` 13

  describe "Problem 5" $ do
    it "myReverse \"A man, a plan, a canal, panama!\" == \"!amanap ,lanac a ,nalp a ,nam A\"" $ do
      problem5 "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

    it "myReverse [1, 2, 3, 4] == [4, 3, 2, 1]" $ do
      problem5 [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]
