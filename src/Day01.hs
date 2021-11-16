module Day01
  ( part1,
    part2,
  )
where

fuelForModule :: Int -> Int
fuelForModule x
  | x >= 6 = div x 3 - 2
  | otherwise = 0

fuelForModule2 :: Int -> Int
fuelForModule2 x
  | x <= 0 = 0
  | otherwise = fuelForModule x + fuelForModule2 (fuelForModule x)

part1 :: [Int] -> Int
part1 = sum . fmap fuelForModule

part2 :: [Int] -> Int
part2 = sum . fmap fuelForModule2
