module Day08 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

type Location = (Int, Int)
type Antennae = Map Char [Location]
type Antinodes = Map Char [Location]

getInput :: IO Text
getInput = readFile "input-08"

example :: IO Text
example =
    return . pack $
        "............\n\
        \........0...\n\
        \.....0......\n\
        \.......0....\n\
        \....0.......\n\
        \......A.....\n\
        \............\n\
        \............\n\
        \........A...\n\
        \.........A..\n\
        \............\n\
        \............"

parse :: Text -> Antennae
parse t =
    Map.fromListWith
        (++)
        [ (c, [(i, j)])
        | (j, l) <- zip [1 ..] (Text.lines t)
        , (i, c) <- zip [1 ..] (unpack l)
        , c /= '.'
        ]

task1 :: Antennae -> Int
task1 = undefined

task2 :: Antennae -> Int
task2 = undefined

-- main :: IO ()
-- main = do
--     input <- parse <$> getInput
--     putStrLn $ "task 1 answer: " <> show (task1 input)
--     putStrLn $ "task 2 answer: " <> show (task2 input)
