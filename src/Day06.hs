module Day06
  ( part1,
    part2,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type OrbitalMap = Map.Map Text.Text (Set.Set Text.Text)

addVertex :: Ord a => a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
addVertex vertex Nothing = Just $ Set.singleton vertex
addVertex vertex (Just rest) = Just $ Set.insert vertex rest

sumOrbits :: OrbitalMap -> Int
sumOrbits edges = Map.foldr (+) 0 $ Map.map findReachable edges
  where
    findReachable neighbors =
      sum $ map (\x -> 1 + findReachable (Map.findWithDefault Set.empty x edges)) $ Set.elems neighbors

countDistance :: OrbitalMap -> Set.Set Text.Text -> Text.Text -> Text.Text -> Int
countDistance edges visited end start
  | Set.member start visited = 1000000
  | start == end = 0
  | otherwise = Set.foldr min 1000000 $ Set.map ((+ 1) . calcDistanceToEnd) $ Map.findWithDefault Set.empty start edges
  where
    calcDistanceToEnd = countDistance edges (Set.insert start visited) end

buildMap :: ((Text.Text, Text.Text) -> OrbitalMap -> OrbitalMap) -> [Text.Text] -> OrbitalMap
buildMap mapper = foldr (mapper . convert) Map.empty
  where
    convert line = (head objects, last objects) where objects = Text.splitOn (Text.pack ")") line

part1 :: [Text.Text] -> Int
part1 inputs = sumOrbits (buildMap mapper inputs)
  where
    mapper (start, end) = Map.alter (addVertex end) start

part2 :: [Text.Text] -> Int
part2 inputs = countDistance (buildMap mapper inputs) Set.empty (Text.pack "9KL") (Text.pack "QYZ")
  where
    mapper (start, end) = Map.alter (addVertex start) end . Map.alter (addVertex end) start
