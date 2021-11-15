module Day03
  ( part1,
    part2,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

data Direction = L | R | U | D deriving (Read)

type Wire = [(Direction, Int)]

type Point = (Int, Int)

type WirePoints = Map.Map Point Int

convert :: Text.Text -> (Direction, Int)
convert text = let string = Text.unpack text in (read [head string], read $ tail string)

shift :: Point -> Direction -> Int -> Point
shift (x, y) L offset = (x - offset, y)
shift (x, y) R offset = (x + offset, y)
shift (x, y) U offset = (x, y - offset)
shift (x, y) D offset = (x, y + offset)

toPoints :: Int -> Point -> Wire -> WirePoints
toPoints n point ((direction, offset) : points) =
  Map.union (Map.fromList $ zip newPoints [n ..]) (toPoints (n + offset) (last newPoints) points)
  where
    newPoints = map (shift point direction) [1 .. offset]
toPoints _ _ [] = Map.empty

solver :: (WirePoints -> [Int]) -> [[Text.Text]] -> Int
solver mapper wires = minimum $ mapper $ Map.intersectionWith (+) firstWire secondWire
  where
    firstWire = toPoints 1 (0, 0) $ map convert $ head wires
    secondWire = toPoints 1 (0, 0) $ map convert $ last wires

part1 :: [[Text.Text]] -> Int
part1 = solver (map (\(x, y) -> abs x + abs y) . Map.keys)

part2 :: [[Text.Text]] -> Int
part2 = solver Map.elems
