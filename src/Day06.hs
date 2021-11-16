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

type OrbitalMap = Map.Map Text.Text (Set.Set Text.Text)

addVertex :: Ord a => a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
addVertex vertex Nothing = Just $ Set.singleton vertex
addVertex vertex (Just rest) = Just $ Set.insert vertex rest

buildMap1 :: [(Text.Text, Text.Text)] -> OrbitalMap
buildMap1 = foldr (\(start, end) -> Map.alter (addVertex end) start) Map.empty

sumOrbits :: OrbitalMap -> Int
sumOrbits edges = Map.foldr (+) 0 $ Map.map findReachable edges
  where
    findReachable neighbors =
      sum $ map (\x -> 1 + findReachable (Map.findWithDefault Set.empty x edges)) $ Set.elems neighbors

buildMap2 :: [(Text.Text, Text.Text)] -> OrbitalMap
buildMap2 = foldr (\(start, end) -> Map.alter (addVertex start) end . Map.alter (addVertex end) start) Map.empty

countDistance :: OrbitalMap -> Set.Set Text.Text -> Text.Text -> Text.Text -> Int
countDistance edges visited end start
  | Set.member start visited = 1000000
  | start == end = 0
  | otherwise = Set.foldr min 1000000 $ Set.map ((+ 1) . calcDistanceToEnd) $ Map.findWithDefault Set.empty start edges
  where
    calcDistanceToEnd = countDistance edges (Set.insert start visited) end

part1 :: [Text.Text] -> Int
part1 inputs = sumOrbits . buildMap1 $ map convert inputs

part2 :: [Text.Text] -> Int
part2 inputs = countDistance (buildMap2 $ map convert inputs) Set.empty (Text.pack "9KL") (Text.pack "QYZ")
