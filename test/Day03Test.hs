module Day03Test
  ( day03Test,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day03
import Test.Hspec
import Util

day03Test :: IO ()
day03Test = hspec $ do
  describe "Day 03" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day03.txt")
      part1 (readStringsCommaDelimited input) `shouldBe` (303 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day03.txt")
      part2 (readStringsCommaDelimited input) `shouldBe` (11222 :: Int)
