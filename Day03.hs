module Day02 where

import Data.Text (Text, concat, pack, splitOn, unpack)
import Data.Text.IO (readFile)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Prelude hiding (lines, readFile, words)

getInput :: IO Text
getInput = readFile "input-03"

example1 :: IO Text
example1 =
    return . pack $ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example2 :: IO Text
example2 = return . pack $ "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

task1 :: Text -> Int
task1 t =
    let number :: String
        number = "[0-9][0-9]?[0-9]?"
        multiplication :: String
        multiplication = "mul\\(" <> number <> "," <> number <> "\\)"
        matches :: [Text]
        matches = getAllTextMatches $ t =~ multiplication
        operands :: Text -> [Text]
        operands t' = getAllTextMatches (t' =~ number)
     in sum $ map (product . map (read . unpack) . operands) matches

task2 :: Text -> Int
task2 t =
    let ds = splitOn (pack "don't()") t
        ds' = map (Data.Text.concat . drop 1 . splitOn (pack "do()")) ds
     in task1 . Data.Text.concat $ take 1 ds ++ ds'

main :: IO ()
main = do
    input <- getInput
    putStrLn $ "task 1 answer: " <> show (task1 input)
    putStrLn $ "task 2 answer: " <> show (task2 input)
