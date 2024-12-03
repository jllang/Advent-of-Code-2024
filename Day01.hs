{-# LANGUAGE LambdaCase #-}

module Day01 where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Text (Text, lines, unpack, words)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile, words)

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
