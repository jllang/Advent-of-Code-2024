{-# GHC_OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Day06 where

import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO
import qualified Day04
import Prelude hiding (Either (..), readFile)

type Location = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show)
type Guard = (Location, Direction)
type Path = [Location]
type Clear = Bool
type Graph = Array Location Clear
type State = (Graph, Guard, Path)

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
        path = []
     in (graph, (fst start, direction), path)
