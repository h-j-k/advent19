module Day06Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day06
import Test.Hspec
import TestUtil

runTest :: IO ()
runTest = hspec $ do
  describe "Day 06" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day06.txt")
      part1 input `shouldBe` (308790 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day06.txt")
      part2 input `shouldBe` (0 :: Int)
