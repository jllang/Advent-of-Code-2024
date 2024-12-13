module Day09 where

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Prelude hiding (readFile)

data State = File | Free
type Input = Vector (Int, Int)

getInput :: IO Text
getInput = readFile "input-09"

example :: IO Text
example = return $ pack "2333133121414131402"

parse :: Text -> Input
parse t =
    let tokenizer c (s, i, is) = case (s, digitToInt c) of
            (File, n) -> (Free, i + 1, (i, n) : is)
            (Free, n) -> (File, i, (-1, n) : is)
     in Text.foldr' tokenizer (File, 0, []) t
            & (\(_, _, is) -> is)
            & Vector.fromList
            & Vector.reverse

compact :: Input -> Input
compact = undefined

checksum :: Input -> Int
checksum =
    let f j (i, n) = (+ i * (j + ((n - 1) * n `div` 2)))
     in Vector.ifoldr' f 0

task1 :: Input -> Int
task1 = checksum . compact
