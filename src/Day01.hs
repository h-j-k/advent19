module Day01
  ( part1,
    part2,
  )
where

part1 :: [Int] -> Int
part1 inputs = sum (map fuelForModule inputs)

fuelForModule :: Int -> Int
fuelForModule x 
 | x >= 6 = div x 3 - 2
 | otherwise = 0 

part2 :: [Int] -> Int
part2 inputs = sum (map fuelForModule2 inputs)

fuelForModule2 :: Int -> Int
fuelForModule2 x
  | x <= 0 = 0
  | otherwise = fuelForModule x + fuelForModule2(fuelForModule x)
