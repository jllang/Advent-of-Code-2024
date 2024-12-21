{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day11 where

import Control.Monad (mapM_, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Char (digitToInt, intToDigit)
import Data.Function ((&))
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mut
import Debug.Trace
import Prelude

type Stone = String
type Fragment = [Stone]
type Line = [Stone]

getInput :: IO String
getInput = readFile "input-11"

example :: IO String
example = return "125 17"

parse :: String -> Line
parse = words

transform :: Stone -> Fragment
transform s =
    case s of
        "0" -> ["1"]
        _ | l `mod` 2 == 0 -> [take l' s, drop l' s]
        _ -> [show (2024 * (read s :: Int))]
  where
    l = length s
    l' = l `div` 2

step :: Line -> Line
step = concatMap transform

task1 :: Line -> Int
task1 = length . (!! 25) . iterate step

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 1 answer: " <> show (task1 input)
