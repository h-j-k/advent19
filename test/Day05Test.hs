module Day05Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day05
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 05" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day05.txt")
      part1 (readNumbersCommaDelimited input) `shouldBe` (10987514 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day05.txt")
      part2 (readNumbersCommaDelimited input) `shouldBe` (14195011 :: Int)
