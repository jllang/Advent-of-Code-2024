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
type MutableInput = Mut.MVector Mut.RealWorld Int

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

isFree :: Int -> Bool
isFree = (-1 ==)

moveFile :: Int -> Int -> MutableInput -> IO MutableInput
moveFile from to mut
    | from < to = do
        i <- Mut.unsafeRead mut from
        j <- Mut.unsafeRead mut to
        case (isFree i, isFree j) of
            (True, True) ->
                moveFile from (to - 1) mut
            (True, False) -> do
                Mut.unsafeSwap mut from to
                moveFile (from + 1) (to - 1) mut
            (False, True) ->
                moveFile (from + 1) (to - 1) mut
            (False, False) ->
                moveFile (from + 1) to mut
moveFile _ _ mut = return mut

compact :: Input -> IO Input
compact is =
    Vector.unsafeThaw is
        >>= moveFile 0 (Vector.length is - 1)
        >>= Vector.unsafeFreeze
        >>= return . Vector.takeWhile (>= 0)

checksum :: Input -> Int
checksum = Vector.ifoldl' (\s i j -> (s + i * j)) 0

task1 :: Input -> IO Int
task1 = return . checksum <=< compact

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn . ("task 1 answer: " <>) . show =<< task1 input
