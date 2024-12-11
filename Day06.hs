{-# GHC_OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Day06 where

import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (second)
import Data.List (find, nub)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Prelude hiding (Either (..), readFile)

type Location = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show)
type Guard = (Location, Direction)
type Path = [Location]
type Clear = Bool
type Graph = Array Location Clear
type State = (Graph, Guard)

getInput :: IO Text
getInput = readFile "input-06"

example :: IO Text
example =
    return . pack $
        "....#.....\n\
        \.........#\n\
        \..........\n\
        \..#.......\n\
        \.......#..\n\
        \..........\n\
        \.#..^.....\n\
        \........#.\n\
        \#.........\n\
        \......#..."

parse :: Text -> State
parse t =
    let ls = map unpack $ Text.lines t
        dim = ((1, 1), (length ls, length (head ls)))
        elems = [((i, j), b) | (i, bs) <- zip [1 ..] ls, (j, b) <- zip [1 ..] bs]
        graph = array dim $ map (second (/= '#')) elems
        start = (\(Just x) -> x) $ find ((`elem` "^>v<") . snd) elems
        direction = case snd start of
            '^' -> Up
            '>' -> Right
            'v' -> Down
            '<' -> Left
     in (graph, (fst start, direction))

turn :: Direction -> Direction
turn d = case d of
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up

step :: Location -> Direction -> Location
step (i, j) d = case d of
    Up -> (i - 1, j)
    Right -> (i, j + 1)
    Down -> (i + 1, j)
    Left -> (i, j - 1)

task1 :: State -> Int
task1 (g, s) =
    let (_, (rows, cols)) = bounds g
        safe (i, j) = and [i > 0, i <= rows, j > 0, j <= cols]
        go (ix, d) =
            let ix' = step ix d
                d' = turn d
                ix'' = step ix d'
             in case (safe ix', safe ix'') of
                    (False, _) -> [(ix, d)]
                    (_, _) | g ! ix' -> (ix, d) : go (ix', d)
                    (_, True) -> (ix, d) : go (ix'', d')
     in length . nub . map fst $ go s

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
