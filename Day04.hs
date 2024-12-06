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

task1 :: String -> Int
task1 t =
    let a :: Array (Int, Int) Char
        a = parse t
        (_, (rows, cols)) = bounds a
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
            count $
                [ (i, j)
                | i <- [1 .. rows]
                , j <- [1 .. cols]
                ]
        vert =
            count $
                [ (i, j)
                | j <- [1 .. cols]
                , i <- [1 .. rows]
                ]
        diag =
            count $
                sortBy
                    (comparing (\(i, j) -> i - j))
                    [ (i, j)
                    | i <- [1 .. rows]
                    , j <- [1 .. cols]
                    , i - j > -(cols - 3)
                    , j - i > -(rows - 3)
                    ]
        anti =
            count $
                sortBy
                    (comparing (\(i, j) -> i + j))
                    [ (i, j)
                    | i <- [1 .. rows]
                    , j <- [1 .. cols]
                    , i + j > 4
                    , i + j < rows + cols - 2
                    ]
     in horiz + vert + diag + anti

task2 :: String -> Int
task2 t =
    undefined

main :: IO ()
main = do
    input <- getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
