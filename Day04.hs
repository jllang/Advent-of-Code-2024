module Day04 where

import Data.Array (Array, array, bounds, (!))
import Data.List (sortBy, tails)
import Data.Ord (comparing)
import Prelude

getInput :: IO String
getInput = readFile "input-04"

example :: IO String
example =
    return $
        "MMMSXXMASM\n\
        \MSAMXMSMSA\n\
        \AMXSXMAAMM\n\
        \MSAMASMSMX\n\
        \XMASAMXAMM\n\
        \XXAMMXXAMA\n\
        \SMSMSASXSS\n\
        \SAXAMASAAA\n\
        \MAMMMXMMMM\n\
        \MXMXAXMASX"

parse :: String -> Array (Int, Int) Char
parse t =
    let ls = lines t
        dim = ((1, 1), (length ls, length (head ls)))
        elems = [((i, j), c) | (i, l) <- zip [1 ..] ls, (j, c) <- zip [1 ..] l]
     in array dim elems

task1 :: Array (Int, Int) Char -> Int
task1 a =
    let (_, (rows, cols)) = bounds a
        allSame xs = and $ map (== head xs) (tail xs)
        count is =
            length
                [ ()
                | cs <- map (take 4) (tails is)
                , length cs == 4
                , let word = map (a !) cs
                      js' = map snd cs
                      is' = map fst cs
                      sums = map (uncurry (+)) cs
                      difs = map (\(x, y) -> abs (x - y)) cs
                , allSame js' || allSame is' || allSame sums || allSame difs
                , word == "XMAS" || word == "SAMX"
                ]
        horiz =
            [ (i, j)
            | i <- [1 .. rows]
            , j <- [1 .. cols]
            ]
        vert =
            [ (i, j)
            | j <- [1 .. cols]
            , i <- [1 .. rows]
            ]
        diag =
            sortBy
                (comparing (\(i, j) -> i - j))
                [ (i, j)
                | i <- [1 .. rows]
                , j <- [1 .. cols]
                -- , i - j > -(cols - 3)
                -- , j - i > -(rows - 3)
                ]
        anti =
            sortBy
                (comparing (\(i, j) -> i + j))
                [ (i, j)
                | i <- [1 .. rows]
                , j <- [1 .. cols]
                -- , i + j > 4
                -- , i + j < rows + cols - 2
                ]
     in count $ horiz ++ vert ++ diag ++ anti

task2 :: Array (Int, Int) Char -> Int
task2 a =
    let (_, (rows, cols)) = bounds a
        add (w, x) (y, z) = (w + y, x + z)
        offsets = [(i, j) | i <- [0 .. rows - 3], j <- [0 .. cols - 3]]
        shape = [(1, 1), (1, 3), (2, 2), (3, 1), (3, 3)]
     in length
            [ ()
            | o <- offsets
            , let word = map ((a !) . (add o)) shape
            , word `elem` ["SSAMM", "MSAMS", "MMASS", "SMASM"]
            ]

main :: IO ()
main = do
    array <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 array)
    putStrLn $ "task 2 answer: " <> show (task2 array)
