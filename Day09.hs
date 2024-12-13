{-# LANGUAGE LambdaCase #-}

module Day09 where

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Char (digitToInt, intToDigit)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mut
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

compact :: Input -> IO Input
compact is =
    let go from to mut
            | from < to = do
                i <- Mut.read mut from
                j <- Mut.read mut to
                case (i, j >= 0) of
                    (-1, True) -> do
                        Mut.swap mut from to
                        go (from + 1) (to - 1) mut
                    (_, True) ->
                        go (from + 1) to mut
                    (_, False) ->
                        go from (to - 1) mut
        go _ _ mut = return mut
     in Vector.thaw is
            >>= go 0 (Vector.length is - 1)
            >>= Vector.freeze

checksum :: Input -> Int
checksum = Vector.ifoldr' (\j i -> (+ j * i)) 0

task1 :: Input -> IO Int
task1 = return . checksum <=< compact
