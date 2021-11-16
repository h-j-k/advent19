module Day05
  ( part1,
    part2,
  )
where

import qualified Data.Array as Array

intCode :: Int -> Array.Array Int Int -> Maybe (Array.Array Int Int)
intCode p instructions =
  let value = instructions Array.! p
      opCode = value `mod` 100
      mode1 = (div value 100 `mod` 10) == 0
      mode2 = (div value 1000 `mod` 10) == 0
   in case opCode of
        99 -> Just instructions
        _ ->
          let nextP = p + 1 + paramCount opCode
           in if nextP <= snd (Array.bounds instructions)
                then
                  let newProgram = case opCode of
                        1 -> Just (process mode1 mode2 (+))
                        2 -> Just (process mode1 mode2 (*))
                        3 -> Just $ instructions Array.// [(get False (p + 1), 1)]
                        4 -> Just instructions
                        _ -> Nothing
                   in newProgram >>= intCode nextP
                else Nothing
  where
    paramCount 1 = 3
    paramCount 2 = 3
    paramCount 3 = 1
    paramCount 4 = 1
    paramCount _ = 0

    get True x = instructions Array.! (instructions Array.! x)
    get False x = instructions Array.! x

    process mode1 mode2 mapper =
      instructions Array.// [(get False (p + 3), mapper (get mode1 (p + 1)) (get mode2 (p + 2)))]

part1 :: [Int] -> Int
part1 inputs =
  case intCode 0 (Array.listArray (0, length inputs - 1) inputs) of
    Just answer -> maximum answer
    Nothing -> error "Error!"

part2 :: [Int] -> Int -> Int
part2 _ _ = 0
