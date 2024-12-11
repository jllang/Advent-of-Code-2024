{-# LANGUAGE LambdaCase #-}

module Day05 where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
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
                <$> (getAllTextMatches (t =~ r))
        rule r m =
            case r of
                [lhs, rhs] -> Map.insertWith (<>) lhs [rhs] m
                _ -> error "malformed rule"
     in ( foldr rule Map.empty (tokenize "[0-9]+\\|[0-9]+" '|')
        , tokenize "([0-9]+,)+[0-9]+" ','
        )

task1 :: ParseResult -> Int
task1 (m, us) =
    let middle xs = head $ drop (length xs `div` 2) xs
        valid u = case u of
            (p : q : ps) ->
                case (q `elem`) <$> m !? p of
                    Just True -> valid (q : ps)
                    _ -> False
            _ -> True
     in sum . map middle $ filter valid us

main :: IO ()
main = do
    lists <- parse <$> getInput
    putStrLn $ "task 1 answer: " <> show (task1 lists)

-- putStrLn $ "task 2 answer: " <> show (task2 lists)
