{-# OPTIONS_GHC -fdefer-type-errors #-}

module Part2Spec (spec) where

import           Part2
import           Test.Hspec

spec :: Spec
spec = do
  describe "Problem 11" $ do
    it "encodeModified \"aaaabccaadeeee\" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']" $ do
      problem11 "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

  describe "Problem 12" $ do
    it "decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] == \"aaaabccaadeeee\"" $ do
      problem12 [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"
