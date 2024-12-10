{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day05 where

import Control.Monad.Trans.State.Strict (State, get, gets)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, concat, empty, length, lines, pack, partition, split, takeWhile, unpack)
import Data.Text.IO (readFile)
import Debug.Trace
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Prelude hiding (length, lines, readFile, takeWhile)

data Rule = Before Int Int
type Successors = Map Int [Int]
type Update = [Int]
data ParseResult = ParseResult
    { successors :: Successors
    , updates :: [Update]
    }
    deriving (Show)

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
    let tokenize :: Regex -> Delimiter -> [[Int]]
        tokenize r d =
            map (read . unpack) . split (== d)
                <$> (getAllTextMatches (t =~ r))
        rule r m =
            case r of
                [lhs, rhs] -> Map.insertWith (<>) lhs [rhs] m
                _ -> error "malformed rule"
     in trace (show t) $
            ParseResult
                (foldr rule Map.empty (tokenize "[0-9]+\\|[0-9]+" '|'))
                (tokenize "([0-9]+,)+[0-9]+" ',')
