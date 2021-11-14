module Day02
  ( part1,
    part2,
  )
where

import qualified Data.Array as Array

part1 :: [Int] -> Int
part1 inputs =
  case intCodeMod (Array.listArray (0, length inputs - 1) inputs) 1202 of
    Just answer -> answer
    Nothing -> error "Error!"

part2 :: [Int] -> Int -> Int
part2 inputs = part2do (Array.listArray (0, length inputs - 1) inputs) 0

part2do :: Array.Array Int Int -> Int -> Int -> Int
part2do array n target =
  case intCodeMod array n of
    Just answer -> if answer == target then n else part2do array (n + 1) target
    Nothing -> part2do array (n + 1) target

intCodeMod :: Array.Array Int Int -> Int -> Maybe Int
intCodeMod array n =
  case intCode (array Array.// [(1, div n 100), (2, mod n 100)]) 0 of
    Just instructions -> Just (instructions Array.! 0)
    Nothing -> Nothing

intCode :: Array.Array Int Int -> Int -> Maybe (Array.Array Int Int)
intCode instructions p =
  let opCode = instructions Array.! p
   in case opCode of
        99 -> Just instructions
        _ ->
          if (p + 4) <= snd (Array.bounds instructions)
            then
              let a = instructions Array.! (p + 1)
                  b = instructions Array.! (p + 2)
                  c = instructions Array.! (p + 3)
                  x = instructions Array.! a
                  y = instructions Array.! b
               in case opCode of
                    1 -> intCode (instructions Array.// [(c, x + y)]) (p + 4)
                    2 -> intCode (instructions Array.// [(c, x * y)]) (p + 4)
                    _ -> Nothing
            else Nothing
