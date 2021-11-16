module TestUtil
  ( readNumbers,
    readNumbersCommaDelimited,
    readStringsCommaDelimited,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Read as Text

readNumbers :: [Text.Text] -> [Int]
readNumbers text = do
  [case Text.decimal x of Left _ -> error "Not a number"; Right n -> fst n | x <- text]

readNumbersCommaDelimited :: [Text.Text] -> [Int]
readNumbersCommaDelimited text = readNumbers (Text.splitOn (Text.pack ",") `concatMap` text)

readStringsCommaDelimited :: [Text.Text] -> [[Text.Text]]
readStringsCommaDelimited = map (Text.splitOn (Text.pack ","))
