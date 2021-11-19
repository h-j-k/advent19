module Day08
  ( part1,
    part2,
  )
where

import qualified Data.Text as Text
import Debug.Trace

chunk :: Int
chunk = 25 * 6

toNumbers :: Text.Text -> [Int]
toNumbers text = [toInt $ x : "" | x <- Text.unpack text]
  where
    toInt x = read x :: Int

parse :: [Int] -> (Int, Int, Int)
parse = (,,) <$> countOf 0 <*> countOf 1 <*> countOf 2
  where
    countOf n = length . filter (== n)

part1 :: Text.Text -> Int
part1 inputs = ones * twos
  where
    (_, ones, twos) = minimum $ map (parse . toNumbers) $ Text.chunksOf chunk inputs

part2 :: Text.Text -> Int
part2 _ = 0
