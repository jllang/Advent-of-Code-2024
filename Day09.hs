{-# LANGUAGE LambdaCase #-}

module Day09 where

import Data.Bifunctor (first)
import Data.Char (digitToInt, intToDigit)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Debug.Trace
import Prelude hiding (readFile)

data State = File | Free
type Input = Vector Int

getInput :: IO Text
getInput = readFile "input-09"

example :: IO Text
example = return $ pack "2333133121414131402"

parse :: Text -> Input
parse t =
    let tokenizer (s, i, is) c =
            case (s, digitToInt c) of
                (File, n) -> (Free, i + 1, take n (repeat i) ++ is)
                (Free, n) -> (File, i, take n (repeat (-1)) ++ is)
        show' = \case
            (-1) -> '.'
            n -> intToDigit n
     in Text.foldl' tokenizer (File, 0, []) t
            & (\(_, _, is) -> is)
            & Vector.fromList
            & Vector.reverse
            & \v -> trace (map show' $ Vector.toList v) v

compact :: Input -> Input
compact = undefined

checksum :: Input -> Int
checksum = Vector.ifoldr' (\j i -> (+ j * i)) 0

task1 :: Input -> Int
task1 = checksum . compact
