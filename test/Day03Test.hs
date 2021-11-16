module Day03Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day03
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 03" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day03.txt")
      part1 (readStringsCommaDelimited input) `shouldBe` (303 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day03.txt")
      part2 (readStringsCommaDelimited input) `shouldBe` (11222 :: Int)
