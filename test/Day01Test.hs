module Day01Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day01
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 01" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day01.txt")
      part1 (readNumbers input) `shouldBe` (3270717 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day01.txt")
      part2 (readNumbers input) `shouldBe` (4903193 :: Int)
