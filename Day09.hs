{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day09 where

import Control.Monad (mapM_, (<=<))
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Char (digitToInt, intToDigit)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mut
import Debug.Trace
import Prelude hiding (readFile)

data State = File | Free
type Raw = Int
type SegmentId = Int
type Position = Int
type Length = Int
type FileId = Int
type Logical = (FileId, Position, Length)
type Filesystem = Vector Logical
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
        tokenizer (s, i, x, input) c =
            let n = digitToInt c
                x' = x + n
                mkInput raw =
                    MkInput
                        ([(head raw, x, n) | n > 0] ++ input.logical)
                        (take n raw ++ input.raw)
             in case s of
                    File ->
                        (Free, i + 1, x', mkInput $ repeat i)
                    Free ->
                        (File, i, x', mkInput free)
     in Text.foldl' tokenizer (File, 0, 0, MkInput [] []) t
            & (\(_, _, _, input) -> input)
            & bimap convert convert

traceDisk :: String -> Vector Raw -> Vector Raw
traceDisk s v =
    let show' = \case
            (-1) -> "."
            n | n > 9 -> "(" ++ show n ++ ")"
            n -> show n
     in trace (s <> ": " <> concatMap show' (Vector.toList v)) v

isFree :: FileId -> Bool
isFree = (-1 ==)

moveRaw :: Position -> Position -> Disk -> IO Disk
moveRaw from to disk
    | from < to = do
        i <- Mut.unsafeRead disk from
        j <- Mut.unsafeRead disk to
        case (isFree i, isFree j) of
            (True, True) ->
                moveRaw from (to - 1) disk
            (True, False) -> do
                Mut.unsafeSwap disk from to
                moveRaw (from + 1) (to - 1) disk
            (False, True) ->
                moveRaw (from + 1) (to - 1) disk
            (False, False) ->
                moveRaw (from + 1) to disk
moveRaw _ _ disk = return disk

compactRaw :: Input -> IO (Vector Raw)
compactRaw (MkInput _ raw) =
    return (traceDisk "before" raw)
        >>= Vector.unsafeThaw
        >>= moveRaw 0 (Vector.length raw - 1)
        >>= Vector.unsafeFreeze
        >>= return . traceDisk "after" . Vector.takeWhile (>= 0)

checksum :: Vector Raw -> Int
checksum = Vector.ifoldl' (\s i j -> (s + i * j)) 0

task1 :: Input -> IO Int
task1 = return . checksum <=< compactRaw

fill :: Filesystem -> Logical -> Position -> Position -> Disk -> IO (Position, Position)
fill fs (i, x, n) from to disk = do
    let (j, y, k) = fs ! to
    if n < k
        then return (from + 1, to)
        else do
            zip [x .. x + n - 1] [y .. y + k - 1]
                & mapM_ (uncurry (Mut.unsafeSwap disk))
            fill fs (i, x + k, n - k) from (to - 1) disk

moveFiles :: Filesystem -> Position -> Position -> Disk -> IO Disk
moveFiles fs from to disk
    | from < to = do
        let (i, x, n) = fs ! from
            (j, y, k) = fs ! to
        print ((i, x, n), (j, y, k))
        case (isFree i, isFree j) of
            (True, True) ->
                trace "(True, True)" $
                    moveFiles fs from (to - 1) disk
            (True, False) ->
                trace "(True, False)" $
                    if n < k
                        then moveFiles fs from (to - 1) disk
                        else do
                            (from', to') <- fill fs (i, x, n) from to disk
                            moveFiles fs from' to' disk
            (False, True) ->
                trace "(False, True)" $
                    moveFiles fs (from + 1) (to - 1) disk
            (False, False) ->
                trace "(False, False)" $
                    moveFiles fs (from + 1) to disk
moveFiles _ _ _ disk = return disk

compactLogical :: Input -> IO (Vector Raw)
compactLogical (MkInput logical raw) =
    return (traceDisk "before" raw)
        >>= Vector.unsafeThaw
        >>= moveFiles logical 0 (Vector.length logical - 1)
        >>= Vector.unsafeFreeze
        >>= return . traceDisk "after"

checksum' :: Vector Raw -> Int
checksum' =
    Vector.ifoldl' (\s i j -> (s + (if j >= 0 then i * j else 0))) 0

task2 :: Input -> IO Int
task2 = return . checksum' <=< compactLogical

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn . ("task 1 answer: " <>) . show =<< task1 input
    putStrLn . ("task 2 answer: " <>) . show =<< task2 input
