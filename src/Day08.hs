module Day08
  ( part1,
    part2,
  )
where

import Data.List.Split
import qualified Data.Text as Text

width :: Int
width = 25

height :: Int
height = 6

parse :: Text.Text -> [[[Int]]]
parse inputs = map toNumbers (Text.chunksOf (width * height) inputs)
  where
    toInt x = read x :: Int
    toNumbers text = chunksOf width [toInt $ x : "" | x <- Text.unpack text]

part1 :: Text.Text -> Int
part1 inputs = ones * twos
  where
    countOf n = length . filter (== n)
    countOfs = fmap ((,,) <$> countOf 0 <*> countOf 1 <*> countOf 2) concat
    (_, ones, twos) = minimum $ map countOfs $ parse inputs

part2 :: Text.Text -> [String]
part2 inputs = map (map render) (foldl merge (replicate height $ replicate width Nothing) $ parse inputs)
  where
    mergePixel Nothing 2 = Nothing
    mergePixel Nothing n = Just n
    mergePixel c _ = c
    mergeRow currentRow incomingRow = [mergePixel c i | (c, i) <- zip currentRow incomingRow]
    merge current incoming = [mergeRow c i | (c, i) <- zip current incoming]
    render (Just 0) = ' '
    render (Just 1) = 'x'
    render _ = ' '
