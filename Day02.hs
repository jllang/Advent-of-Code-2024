{-# LANGUAGE LambdaCase #-}

module Day02 where

import Data.Bifunctor (bimap)
import Data.Foldable (concatMap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (dropWhile, foldl', group, map, nub, sort, unzip, zip)
import Data.Text (Text, lines, pack, unpack, words)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile, words)

type Report = [Int]
type Level = Int

getInput :: IO Text
getInput = readFile "input-02"

example :: IO Text
example =
    return . pack $
        "7 6 4 2 1\n\
        \1 2 7 8 9\n\
        \9 7 6 2 1\n\
        \1 3 2 4 5\n\
        \8 6 4 4 1\n\
        \1 3 6 7 9\n"

parse :: Text -> [Report]
parse t = map (map (read . unpack) . words) $ lines t

close :: Level -> Level -> Bool
close l1 l2 =
    let d = abs (l1 - l2)
     in d >= 1 && d <= 3

valid :: Report -> Bool
valid ls =
    let inc = l1 < l2 where [l1, l2] = take 2 ls
        helper (l, l')
            | inc = l < l' && close l l'
            | otherwise = l >= l' && close l l'
     in and . map helper . zip ls $ tail ls

task1 :: Text -> Int
task1 = sum . map ((\r -> if valid r then 1 else 0)) . parse

main :: IO ()
main = do
    input <- getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)
    -- putStrLn $ "task 2 answer: " <> show (task2 input)
    return ()
