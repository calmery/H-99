{-# OPTIONS_GHC -fdefer-type-errors #-}

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

  describe "Problem 6" $ do
    it "isPalindrome [1, 2, 3] == False" $ do
      problem6 [1, 2, 3] `shouldBe` False

    it "isPalindrome \"madamimadam\" == True" $ do
      problem6 "madamimadam" `shouldBe` True

    it "isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1] == True" $ do
      problem6 [1, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True

  describe "Problem 7" $ do
    it "flatten (Elem 5) == [5]" $ do
      problem7 (Elem 5) `shouldBe` [5]

    it "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1, 2, 3, 4, 5]" $ do
      problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

    it "flatten (List []) == []" $ do
      problem7 (List []) `shouldBe` []

  describe "Problem 8" $ do
    it "compress \"aaaabccaadeeee\" == \"abcade\"" $ do
      problem8 "aaaabccaadeeee" `shouldBe` "abcade"

  describe "Problem 9" $ do
    it "pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == [\"aaaa\", \"b\", \"cc\", \"aa\", \"d\", \"eeee\"]" $ do
      problem9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

  describe "Problem 10" $ do
    it "encode \"aaaabccaadeeee\" == [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]" $ do
      problem10 "aaaabccaadeeee" `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
