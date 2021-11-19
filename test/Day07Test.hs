module Day07Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day07
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 07" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day07.txt")
      part1 (readNumbersCommaDelimited input) `shouldBe` (272368 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day07.txt")
      part2 (readNumbersCommaDelimited input) `shouldBe` (19741286 :: Int)
