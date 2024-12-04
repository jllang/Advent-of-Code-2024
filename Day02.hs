{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-x-partial #-}

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
     in 1 <= d && d <= 3

valid :: Report -> Bool
valid ls =
    let inc = l1 < l2 where [l1, l2] = take 2 ls -- We assume `length ls >= 2`
        helper (l, l')
            | inc = l < l' && close l l'
            | otherwise = l >= l' && close l l'
     in and . map helper . zip ls $ tail ls -- We assume `length ls >= 2`

fromBool :: Bool -> Int
fromBool b
    | b = 1
    | otherwise = 0

task1 :: [Report] -> Int
task1 =
    sum . map (fromBool . valid)

task2 :: [Report] -> Int
task2 =
    let removeDiagonal r =
            [take i r ++ drop (i + 1) r | i <- [0 .. length r]]
     in sum . map (fromBool . or . map valid . removeDiagonal)

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)
    putStrLn $ "task 2 answer: " <> show (task2 input)
    return ()
