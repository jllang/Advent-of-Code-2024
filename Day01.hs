{-# LANGUAGE LambdaCase #-}

module Day01 where

import Data.Bifunctor (bimap)
import Data.Foldable (concatMap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (dropWhile, group, map, nub, sort, unzip, zip)
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

sortInputs :: Text -> ([Int], [Int])
sortInputs input =
    let split = \case
            [a, b] -> (a, b)
            _ -> error "Each line must have exactly two numbers in it"
     in lines input
            & map (split . map (read . unpack) . words)
            & unzip
            & bimap sort sort

task1 :: Text -> Int
task1 =
    sum . map (\(x, y) -> abs (x - y)) . uncurry zip . sortInputs

task2 :: Text -> Int
task2 inputs =
    let helper :: [[Int]] -> [[Int]] -> [Int]
        helper (xs@(x : _) : xss) yss = case dropWhile ((< x) . head) yss of
            (zs@(z : _) : zss)
                | z == x ->
                    length xs * z * length zs : helper xss yss
            (_ : zss) ->
                0 : helper xss yss
            _ ->
                []
        helper [] _ = []
     in sortInputs inputs
            & bimap group group
            & uncurry helper
            & sum
