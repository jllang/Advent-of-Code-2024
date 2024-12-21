{-# LANGUAGE LambdaCase #-}

module Day13 where

import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Ratio
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Debug.Trace
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import Prelude hiding (readFile)

type Row a = (a, a, a)
type Pair a = (Row a, Row a)
type Solution a = (a, a)

example :: IO Text
example =
    return . pack $
        "Button A: X+94, Y+34\n\
        \Button B: X+22, Y+67\n\
        \Prize: X=8400, Y=5400\n\
        \\n\
        \Button A: X+26, Y+66\n\
        \Button B: X+67, Y+21\n\
        \Prize: X=12748, Y=12176\n\
        \\n\
        \Button A: X+17, Y+86\n\
        \Button B: X+84, Y+37\n\
        \Prize: X=7870, Y=6450\n\
        \\n\
        \Button A: X+69, Y+23\n\
        \Button B: X+27, Y+71\n\
        \Prize: X=18641, Y=10279"

getInput :: IO Text
getInput = readFile "input-13"

parse :: Text -> [Pair (Ratio Int)]
parse t =
    let prefixed prefix p = Parsec.string prefix *> p
        num prefix = fromIntegral . read <$> (prefixed prefix (Parsec.many1 Parsec.digit))
        mkPair (ax, ay) (bx, by) (x, y) = ((ax, bx, x), (ay, by, y))
        emptyLine = Parsec.string "\n\n"
        parser =
            mkPair
                <$> ((,) <$> num "Button A: X+" <*> num ", Y+")
                <*> (prefixed "\n" ((,) <$> num "Button B: X+" <*> num ", Y+"))
                <*> (prefixed "\n" ((,) <$> num "Prize: X=" <*> num ", Y="))
     in case Parsec.parse (parser `Parsec.sepBy` emptyLine) "input" t of
            Left _ -> []
            Right xs -> xs

solve :: (Eq a, Fractional a) => Pair a -> Maybe (Solution a)
solve ((a11, a12, b1), (a21, a22, b2)) = do
    let c1 = a21 / a11
        a22' = a22 - c1 * a12
        b2' = b2 - c1 * b1
    guard $ a22' /= 0
    let c2 = a12 / a22'
        b1' = b1 - c2 * b2'
    return (b1' / a11, b2' / a22')

task1 :: [Pair (Ratio Int)] -> Int
task1 input =
    map solve input
        & catMaybes
        & filter (\(a, b) -> denominator a * denominator b == 1)
        & map (uncurry (+) . (bimap ((3 *) . numerator) numerator))
        & sum

main :: IO ()
main = do
    input <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)

-- putStrLn $ "task 2 answer: " <> show (task2 input)
