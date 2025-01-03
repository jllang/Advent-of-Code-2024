{-# LANGUAGE LambdaCase #-}

module Day05 where

import Data.List (partition, sortBy)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Ordering)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.IO (readFile)
import Debug.Trace
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Prelude hiding (readFile)

type Page = Int
type Successors = Map Page [Page]
type Update = [Page]
type ParseResult = (Successors, [Update])

getInput :: IO Text
getInput = readFile "input-05"

example :: IO Text
example =
    return . pack $
        "47|53\n\
        \97|13\n\
        \97|61\n\
        \97|47\n\
        \75|29\n\
        \61|13\n\
        \75|53\n\
        \29|13\n\
        \97|29\n\
        \53|29\n\
        \61|53\n\
        \97|53\n\
        \61|29\n\
        \47|13\n\
        \75|47\n\
        \97|75\n\
        \47|61\n\
        \75|61\n\
        \47|29\n\
        \75|13\n\
        \53|13\n\
        \\n\
        \75,47,61,53,29\n\
        \97,61,53,29,13\n\
        \75,29,13\n\
        \75,97,47,61,53\n\
        \61,13,29\n\
        \97,13,75,29,47"

type Regex = String
type Delimiter = Char

parse :: Text -> ParseResult
parse t =
    let tokenize :: Regex -> Delimiter -> [[Page]]
        tokenize r d =
            map (read . unpack) . Text.split (== d)
                <$> getAllTextMatches (t =~ r)
        rule r m =
            case r of
                [lhs, rhs] -> Map.insertWith (<>) lhs [rhs] m
                _ -> error "malformed rule"
     in ( foldr rule Map.empty (tokenize "[0-9]+\\|[0-9]+" '|')
        , tokenize "([0-9]+,)+[0-9]+" ','
        )

before :: Successors -> Page -> Page -> Ordering
before m p q
    | p == q = EQ
    | fromMaybe False (elem q <$> m !? p) = LT
    | otherwise = GT

valid :: Successors -> Update -> Bool
valid m us = us == sortBy (before m) us

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

task1 :: ParseResult -> Int
task1 (m, us) = sum . map middle $ filter (valid m) us

task2 :: ParseResult -> Int
task2 (m, us) =
    let ws = filter (not . valid m) us
     in sum $ map (middle . sortBy (before m)) ws

main :: IO ()
main = do
    updates <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 updates)
    putStrLn $ "task 2 answer: " <> show (task2 updates)
