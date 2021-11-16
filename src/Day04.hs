module Day04
  ( part1,
    part2,
  )
where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

base :: Int
base = 10

digits :: [Int] -> Int -> [Int]
digits list n
  | n < base = n : list
  | otherwise = digits ((n `mod` base) : list) (n `div` base)

isNotDecreasing :: [Int] -> Bool
isNotDecreasing xs = base /= foldl (\a b -> if b < a then base else b) (-1) xs

solver :: ([Int] -> Bool) -> String -> Int
solver predicate input = length . filter isValid $ map (digits []) [(head range) .. (last range)]
  where
    parsed = Text.splitOn (Text.pack "-") (Text.pack input)
    range = [case Text.decimal x of Left _ -> error "Not a number"; Right n -> fst n | x <- parsed]
    isValid = (&&) <$> isNotDecreasing <*> fmap (any predicate) List.group

part1 :: String -> Int
part1 = solver (\x -> 2 <= length x)

part2 :: String -> Int
part2 = solver (\x -> 2 == length x)
