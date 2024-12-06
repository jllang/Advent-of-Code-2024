{-# LANGUAGE LambdaCase #-}

module Day01 where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.List (group, sort)
import Data.Text (Text, lines, pack, unpack, words)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile, words)

getInput :: IO Text
getInput = readFile "input-01"

example :: IO Text
example =
    return . pack $
        "3   4\n\
        \4   3\n\
        \2   5\n\
        \1   3\n\
        \3   9\n\
        \3   3"

parse :: Text -> ([Int], [Int])
parse input =
    let split = \case
            [a, b] -> (a, b)
            _ -> error "Each line must have exactly two numbers in it"
     in lines input
            & map (split . map (read . unpack) . words)
            & unzip
            & bimap sort sort

task1 :: ([Int], [Int]) -> Int
task1 =
    sum . map (\(x, y) -> abs (x - y)) . uncurry zip

task2 :: ([Int], [Int]) -> Int
task2 lists =
    let helper :: [[Int]] -> [[Int]] -> [Int]
        helper (xs@(x : _) : xss) yss = case dropWhile ((< x) . head) yss of
            (zs@(z : _) : zss)
                | z == x ->
                    length xs * z * length zs : helper xss zss
            (_ : zss) ->
                helper xss yss
            _ ->
                []
        helper [] _ = []
     in bimap group group lists
            & uncurry helper
            & sum

main :: IO ()
main = do
    lists <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 lists)
    putStrLn $ "task 2 answer: " <> show (task2 lists)
