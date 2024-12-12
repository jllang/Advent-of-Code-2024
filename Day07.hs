{-# LANGUAGE LambdaCase #-}

module Day07 where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Debug.Trace
import Prelude hiding (readFile)

data Expr = Const Int | Add Expr Expr | Mul Expr Expr

instance Show Expr where
    show (Const n) = show n
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

eval :: Expr -> Int
eval e = case e of
    Const n -> n
    Add x y -> eval x + eval y
    Mul x y -> eval x * eval y

type Input = [(Int, [Int])]

getInput :: IO Text
getInput = readFile "input-07"

example :: IO Text
example =
    return . pack $
        "190: 10 19\n\
        \3267: 81 40 27\n\
        \83: 17 5\n\
        \156: 15 6\n\
        \7290: 6 8 6 15\n\
        \161011: 16 10 13\n\
        \192: 17 8 14\n\
        \21037: 9 7 18 13\n\
        \292: 11 6 16 20"

tread :: (Read a) => Text -> a
tread = read . unpack

parse :: Text -> Input
parse t =
    let p = not . (== ':')
        ls = Text.lines t
        rs = map (tread . Text.takeWhile p) ls
        os = map (map tread . tail . Text.split (== ' ') . Text.takeWhileEnd p) ls
     in zip rs os

task1 :: Input -> Int
task1 =
    let exprs = \case
            [x] -> [Const x]
            (x : xs) -> [op y (Const x) | op <- [Add, Mul], y <- exprs xs]
        valid (z, xs) =
            reverse xs
                & exprs
                & map eval
                & elem z
     in sum . map fst . filter valid

main :: IO ()
main = do
    lists <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 lists)

-- putStrLn $ "task 2 answer: " <> show (task2 lists)
