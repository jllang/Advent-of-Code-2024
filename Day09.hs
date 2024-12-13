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
getInput = do
    t <- readFile "input-09"
    return $ case Text.unsnoc t of
        Just (t', '\n') -> t'
        _ -> t

example :: IO Text
example = return $ pack "2333133121414131402"

parse :: Text -> Input
parse t =
    let free = repeat (-1)
        tokenizer (s, i, is) c =
            case (s, digitToInt c) of
                (File, n) -> (Free, i + 1, take n (repeat i) ++ is)
                (Free, n) -> (File, i, take n free ++ is)
     in Text.foldl' tokenizer (File, 0, []) t
            & (\(_, _, is) -> is)
            & Vector.fromList
            & Vector.reverse

compact :: Input -> Input
compact = undefined

checksum :: Input -> Int
checksum = Vector.ifoldr' (\j i -> (+ j * i)) 0

task1 :: Input -> Int
task1 = checksum . compact
