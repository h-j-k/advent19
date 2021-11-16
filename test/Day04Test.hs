module Day04Test
  ( runTest,
  )
where

import Day04
import Test.Hspec

runTest :: IO ()
runTest = hspec $ do
  describe "Day 04" $ do
    it "Part 1" $ do
      part1 "145852-616942" `shouldBe` (1767 :: Int)

    it "Part 2" $ do
      part2 "145852-616942" `shouldBe` (1192 :: Int)
