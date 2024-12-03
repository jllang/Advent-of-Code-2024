{-# LANGUAGE LambdaCase #-}

module Day01 where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Text (Text, lines, unpack, words)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile, words)

task_1_1 :: IO Int
task_1_1 =
    let split :: [Int] -> (Int, Int)
        split = \case
            [a, b] -> (a, b)
            _ -> error "Each line must have exactly two numbers in it"
     in readFile "input-1-1" <&> \input ->
            lines input
                & map (split . map (read . unpack) . words)
                & unzip
                & bimap sort sort
                & uncurry zip
                & map (\(x, y) -> abs (x - y))
                & sum
