module Day02Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day02
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 02" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day02.txt")
      part1 (readNumbersCommaDelimited input) `shouldBe` (9581917 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day02.txt")
      part2 (readNumbersCommaDelimited input) (19690720 :: Int) `shouldBe` (2505 :: Int)
