{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day14 where

import Data.Char (intToDigit)
import Data.Function ((&))
import Data.List.Extra (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import Text.Regex.TDFA (getAllTextMatches, (=~))

data Robot = Robot {x :: Int, y :: Int, dx :: Int, dy :: Int} deriving (Show)

example :: IO String
example =
    return
        "p=0,4 v=3,-3\n\
        \p=6,3 v=-1,-3\n\
        \p=10,3 v=-1,2\n\
        \p=2,0 v=2,-1\n\
        \p=0,0 v=1,3\n\
        \p=3,0 v=-2,-2\n\
        \p=7,6 v=-1,-3\n\
        \p=3,0 v=-1,-2\n\
        \p=9,3 v=2,3\n\
        \p=7,3 v=-1,2\n\
        \p=2,4 v=2,-3\n\
        \p=9,5 v=-3,-3"

getInput :: IO String
getInput = readFile "input-14"

parse :: String -> Vector Robot
parse s =
    (getAllTextMatches (s =~ "-?[0-9]+") :: [String])
        & map read
        & chunksOf 4
        & map (\[x, y, dx, dy] -> Robot x y dx dy)
        & Vector.fromList

type Width = Int
type Height = Int

step :: Width -> Height -> Vector Robot -> Vector Robot
step w h =
    Vector.map
        (\(Robot x y dx dy) -> Robot ((x + dx) `mod` w) ((y + dy) `mod` h) dx dy)

combine :: (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
combine r m = Map.unionWith (+) m (Map.fromList [(r, 1)])

prettyPrint :: Bool -> Width -> Height -> Vector Robot -> String
prettyPrint b w h rs =
    let cs =
            Vector.map (\(Robot x y _ _) -> (x, y)) rs
                & Vector.foldr combine Map.empty
                & Map.map
                    ( \case
                        0 -> "."
                        n | n < 10 -> [intToDigit n]
                        n -> "(" ++ show n ++ ")"
                    )
     in [ c
        | y <- [0 .. h - 1]
        , x <- [0 .. w - 1]
        , let c =
                if b && x == w `div` 2 || y == h `div` 2
                    then " "
                    else Map.findWithDefault "." (x, y) cs
        ]
            & concat
            & chunksOf w
            & unlines

quadrant :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
quadrant cs (x0, y0) (xn, yn) =
    sum
        [ n
        | y <- [y0 .. yn]
        , x <- [x0 .. xn]
        , let n = Map.findWithDefault 0 (x, y) cs
        ]

task1 :: Width -> Height -> Vector Robot -> Int
task1 w h rs =
    let cs =
            iterate (step w h) rs
                & (!! 100)
                -- & Vector.filter (\(Robot x y _ _) -> x /= w `div` 2 && y /= h `div` 2)
                & (\r -> trace (prettyPrint True w h r) r)
                & Vector.map (\(Robot x y _ _) -> (x, y))
                & Vector.foldr combine Map.empty
        topLeft = quadrant cs (0, 0) (w `div` 2 - 1, h `div` 2 - 1)
        topRight = quadrant cs (w `div` 2 + 1, 0) (w - 1, h `div` 2 - 1)
        bottomLeft = quadrant cs (0, h `div` 2 + 1) (w `div` 2 - 1, h - 1)
        bottomRight = quadrant cs (w `div` 2 + 1, h `div` 2 + 1) (w - 1, h - 1)
     in topLeft * topRight * bottomLeft * bottomRight

task2 :: Width -> Height -> Vector Robot -> Int
task2 input = undefined

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 101 103 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
