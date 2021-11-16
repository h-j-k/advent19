module Day05
  ( part1,
    part2,
  )
where

import qualified Data.Array as Array

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

intCode :: Int -> Int -> Array.Array Int Int -> Maybe (Array.Array Int Int)
intCode systemId p instructions =
  let value = instructions Array.! p
      opCode = value `mod` 100
      mode1 = (div value 100 `mod` 10) == 0
      mode2 = (div value 1000 `mod` 10) == 0
   in case opCode of
        99 -> Just instructions
        _ ->
          let nextP = p + 1 + paramCount opCode
           in if nextP <= snd (Array.bounds instructions)
                then case opCode of
                  1 -> intCode systemId (p + 4) (process mode1 mode2 (+))
                  2 -> intCode systemId (p + 4) (process mode1 mode2 (*))
                  3 -> intCode systemId (p + 2) (instructions Array.// [(get False (p + 1), systemId)])
                  4 -> intCode systemId (p + 2) instructions
                  5 ->
                    if get mode1 (p + 1) == 0
                      then intCode systemId (p + 3) instructions
                      else intCode systemId (get mode2 (p + 2)) instructions
                  6 ->
                    if get mode1 (p + 1) /= 0
                      then intCode systemId (p + 3) instructions
                      else intCode systemId (get mode2 (p + 2)) instructions
                  7 -> intCode systemId (p + 4) (process mode1 mode2 (fromEnum `compose2` (<)))
                  8 -> intCode systemId (p + 4) (process mode1 mode2 (fromEnum `compose2` (==)))
                  _ -> Nothing
                else Nothing
  where
    paramCount 1 = 3
    paramCount 2 = 3
    paramCount 3 = 1
    paramCount 4 = 1
    paramCount 5 = 2
    paramCount 6 = 2
    paramCount 7 = 3
    paramCount 8 = 3
    paramCount _ = 0

    get True x = instructions Array.! (instructions Array.! x)
    get False x = instructions Array.! x

    process mode1 mode2 mapper =
      instructions Array.// [(get False (p + 3), mapper (get mode1 (p + 1)) (get mode2 (p + 2)))]

solver :: Int -> [Int] -> Int
solver systemId inputs = case intCode systemId 0 (Array.listArray (0, length inputs - 1) inputs) of
  Just answer -> maximum answer
  Nothing -> error "Error!"

part1 :: [Int] -> Int
part1 = solver 1

part2 :: [Int] -> Int
part2 = solver 5
