module Day06
  ( part1,
    part2,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

convert :: Text.Text -> (Text.Text, Text.Text)
convert line = (head objects, last objects) where objects = Text.splitOn (Text.pack ")") line

buildMap :: [(Text.Text, Text.Text)] -> Map.Map Text.Text (Set.Set Text.Text)
buildMap = foldr addNewEdge Map.empty
  where
    addNewEdge (start, end) = Map.alter (addVertex end) start

    addVertex vertex Nothing = Just $ Set.singleton vertex
    addVertex vertex (Just rest) = Just $ Set.insert vertex rest

sumOrbits :: Map.Map Text.Text (Set.Set Text.Text) -> Map.Map Text.Text Int
sumOrbits edges = Map.map findReachable edges
  where
    findReachable neighbors =
      sum $ map (\x -> 1 + findReachable (Map.findWithDefault Set.empty x edges)) $ Set.elems neighbors

part1 :: [Text.Text] -> Int
part1 inputs = Map.foldr (+) 0 . sumOrbits . buildMap $ map convert inputs

part2 :: [Text.Text] -> Int
part2 _ = 0
