module Day08 where

import Data.Foldable (foldr')
import Data.Function ((&))
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

type Location = (Int, Int)
type Antennae = Map Char [Location]
type Input = (Antennae, Location)
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

parse :: Text -> Input
parse t =
    let ls = Text.lines t
     in ( Map.fromListWith
            (++)
            [ (c, [(i, j)])
            | (i, l) <- zip [1 ..] ls
            , (j, c) <- zip [1 ..] (unpack l)
            , c /= '.'
            ]
        , (length ls, Text.length (head ls))
        )

infixr 7 *:
infixr 6 +:, -:

(+:) :: Location -> (Int, Int) -> Location
(x1, y1) +: (x2, y2) = (x1 + x2, y1 + y2)

(-:) :: Location -> (Int, Int) -> Location
(x1, y1) -: (x2, y2) = (x1 - x2, y1 - y2)

(*:) :: Int -> Location -> Location
n *: (x, y) = (n * x, n * y)

safe :: Location -> Location -> Bool
safe (rows, cols) (x, y) = and [x > 0, x <= cols, y > 0, y <= rows]

solve :: (Location -> Location -> Set Location) -> Antennae -> Int
solve antinodes a =
    let collect freq =
            let ls = a ! freq
             in [antinodes l1 l2 | l1 <- ls, l2 <- ls, l1 /= l2]
                    & foldr' Set.union Set.empty
     in Map.keys a
            & map collect
            & foldr' Set.union Set.empty
            & Set.size

task1 :: Input -> Int
task1 input =
    let safe' = safe (snd input)
        antinodes l1 l2 =
            let d = l2 -: l1
             in Set.fromList $
                    [l | let l = l2 +: d, safe' l] ++ [l | let l = l1 -: d, safe' l]
     in solve antinodes (fst input)

task2 :: Input -> Int
task2 input =
    let antinodes l1 l2 =
            let helper f =
                    [l2 `f` (n *: (l2 -: l1)) | n <- [1 ..]]
                        & takeWhile (safe (snd input))
                        & Set.fromList
             in Set.union (helper (-:)) (helper (+:))
     in solve antinodes (fst input)

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)
    putStrLn $ "task 2 answer: " <> show (task2 input)
