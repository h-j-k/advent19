module Day07
  ( part1,
    part2,
  )
where

import qualified Control.Monad.Writer.Lazy as Writer
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Maybe

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

intCode :: [Int] -> Int -> Writer.Writer [Int] (Array.Array Int Int) -> Maybe [Int]
intCode ampInputs p programState =
  let value = instructions Array.! p
      opCode = value `mod` 100
      mode1 = (div value 100 `mod` 10) == 0
      mode2 = (div value 1000 `mod` 10) == 0
   in case opCode of
        99 -> Just output
        _ ->
          let nextP = p + 1 + paramCount opCode
           in if nextP <= snd (Array.bounds instructions)
                then case opCode of
                  1 -> intCode ampInputs (p + 4) (process mode1 mode2 (+))
                  2 -> intCode ampInputs (p + 4) (process mode1 mode2 (*))
                  3 -> case ampInputs of
                    [] -> Nothing
                    ampInput : rest -> intCode rest (p + 2) $ (>>) programState $ return $ instructions Array.// [(get False (p + 1), ampInput)]
                  4 -> intCode ampInputs (p + 2) $ programState >> Writer.writer (instructions, [get mode1 (p + 1)])
                  5 ->
                    if get mode1 (p + 1) == 0
                      then intCode ampInputs (p + 3) programState
                      else intCode ampInputs (get mode2 (p + 2)) programState
                  6 ->
                    if get mode1 (p + 1) /= 0
                      then intCode ampInputs (p + 3) programState
                      else intCode ampInputs (get mode2 (p + 2)) programState
                  7 -> intCode ampInputs (p + 4) (process mode1 mode2 (fromEnum `compose2` (<)))
                  8 -> intCode ampInputs (p + 4) (process mode1 mode2 (fromEnum `compose2` (==)))
                  _ -> Nothing
                else Nothing
  where
    (instructions, output) = Writer.runWriter programState
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
      (>>) programState $ return $ instructions Array.// [(get False (p + 3), mapper (get mode1 (p + 1)) (get mode2 (p + 2)))]

findSignal :: Int -> Array.Array Int Int -> [Int] -> Maybe Int
findSignal signal _ [] = Just signal
findSignal signal instructions (phase : rest) =
  case intCode [phase, signal] 0 $ Writer.writer (instructions, []) of
    Just [nextSignal] -> findSignal nextSignal instructions rest
    _ -> Nothing

solver :: [Int] -> (Array.Array Int Int -> [Int] -> Maybe Int) -> [Int] -> Int
solver phases processor inputs = maximum $ map (fromMaybe 0 . processor instructions) $ List.permutations phases
  where
    instructions = Array.listArray (0, length inputs - 1) inputs

part1 :: [Int] -> Int
part1 = solver [0 .. 4] (findSignal 0)

part2 :: [Int] -> Int
part2 _ = 0
