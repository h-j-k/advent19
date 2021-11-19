module Day08Test
  ( runTest,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day08
import Test.Hspec

runTest :: IO ()
runTest = hspec $ do
  describe "Day 08" $ do
    it "Part 1" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day08.txt")
      part1 (head input) `shouldBe` (1560 :: Int)

    it "Part 2" $ do
      input <- fmap Text.lines (Text.readFile "./input/Day08.txt")
      part2 (head input) `shouldBe` (0 :: Int)
