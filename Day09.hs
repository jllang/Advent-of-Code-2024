{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day09 where

import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor, bimap, first)
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
type Raw = Int
type Position = Int
type Length = Int
type FileId = Int
type Logical = (FileId, Length)
type Disk = Mut.MVector Mut.RealWorld Raw

data GenInput a b = MkInput {logical :: a, raw :: b}

type Input = GenInput (Vector Logical) (Vector Raw)

instance Bifunctor GenInput where
    bimap f g (MkInput xs ys) = MkInput (f xs) (g ys)

getInput :: IO Text
getInput = do
    t <- readFile "input-09"
    return $ case Text.unsnoc t of
        Just (t', '\n') -> t'
        _ -> t

example :: IO Text
example = return $ pack "2333133121414131402"

convert :: (Vector.Unbox a) => [a] -> Vector a
convert = Vector.reverse . Vector.fromList

parse :: Text -> Input
parse t =
    let free = repeat (-1)
        tokenizer (s, i, input) c =
            let n = digitToInt c
                mkInput raw =
                    MkInput
                        ((i, n) : input.logical)
                        (take n raw ++ input.raw)
             in case s of
                    File ->
                        (Free, i + 1, mkInput $ repeat i)
                    Free ->
                        (File, i, mkInput free)
     in Text.foldl' tokenizer (File, 0, MkInput [] []) t
            & (\(_, _, input) -> input)
            & bimap convert convert

isFree :: FileId -> Bool
isFree = (-1 ==)

moveRaw :: Position -> Position -> Disk -> IO Disk
moveRaw from to mut
    | from < to = do
        i <- Mut.unsafeRead mut from
        j <- Mut.unsafeRead mut to
        case (isFree i, isFree j) of
            (True, True) ->
                moveRaw from (to - 1) mut
            (True, False) -> do
                Mut.unsafeSwap mut from to
                moveRaw (from + 1) (to - 1) mut
            (False, True) ->
                moveRaw (from + 1) (to - 1) mut
            (False, False) ->
                moveRaw (from + 1) to mut
moveRaw _ _ mut = return mut

compactRaw :: Input -> IO (Vector Int)
compactRaw (MkInput _ raw) =
    Vector.unsafeThaw raw
        >>= moveRaw 0 (Vector.length raw - 1)
        >>= Vector.unsafeFreeze
        >>= return . Vector.takeWhile (>= 0)

checksum :: Vector Int -> Int
checksum = Vector.ifoldl' (\s i j -> (s + i * j)) 0

task1 :: Input -> IO Int
task1 = return . checksum <=< compactRaw

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn . ("task 1 answer: " <>) . show =<< task1 input
