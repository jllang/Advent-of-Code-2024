{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02 where

import Data.Bifunctor (bimap)
import Data.Foldable (concatMap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (dropWhile, foldl', group, map, nub, sort, unzip, zip, zipWith)
import Data.Text (Text, lines, pack, unpack, words)
import Data.Text.IO (readFile)
import Text.Regex.Base.RegexLike (makeRegex)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Prelude hiding (lines, readFile, words)

getInput :: IO Text
getInput = readFile "input-03"

example :: IO Text
example =
    return . pack $ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

task1 :: Text -> Int
task1 t =
    let number :: String
        number = "[0-9][0-9]?[0-9]?"
        multiplication :: String
        multiplication = "mul\\(" <> number <> "," <> number <> "\\)"
        matches :: [Text]
        matches = getAllTextMatches $ t =~ multiplication
        operands :: Text -> [Text]
        operands t' = getAllTextMatches (t' =~ number)
     in sum $ map (product . map (read . unpack) . operands) matches

main :: IO ()
main = do
    input <- getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
