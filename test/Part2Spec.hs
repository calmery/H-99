{-# OPTIONS_GHC -fdefer-type-errors #-}

module Part2Spec (spec) where

import Test.Hspec
import Part2

spec :: Spec
spec = do
  describe "Problem 11" $ do
    it "encodeModified \"aaaabccaadeeee\" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']" $ do
      problem11 "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
