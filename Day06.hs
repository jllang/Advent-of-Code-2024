{-# GHC_OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Day06 where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List (find, nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Prelude hiding (Either (..), readFile)

type Location = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Eq, Ord, Show)
type Guard = (Location, Direction)
type Path = [Guard]
type Clear = Bool
type Graph = Array Location Clear
type Configuration = (Graph, Guard)

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

parse :: Text -> Configuration
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

checkBounds :: Int -> Int -> Location -> Bool
checkBounds cols rows (i, j) = and [i > 0, i <= rows, j > 0, j <= cols]

run :: (Location -> Bool) -> (Location -> Bool) -> Guard -> Path
run safe ok (ix, d) =
    let ix' = step ix d
        d' = turn d
        ix'' = step ix d'
     in case (safe ix', safe ix'') of
            (False, _) -> [(ix, d)]
            (_, _) | ok ix' -> (ix, d) : run safe ok (ix', d)
            (_, True) -> (ix, d) : run safe ok (ix'', d')

task1 :: Configuration -> Int
task1 (g, s) =
    run (uncurry checkBounds . snd $ bounds g) (g !) s
        & map fst
        & nub
        & length

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
