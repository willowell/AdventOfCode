module Day01 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

solution :: Solution
{- Parser Output Type -} [[Int]]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
        Parse the input into a list of lists of integers,
        where each sub-list represents the calories each elf is carrying.
      -}
      decodeInput = elfCalories <* eof
    , solveA = defSolver
      {
        {- | Part A Solver
          Which elf is carrying the most calories?
          How many calories are they carrying?
        -}
        solve = Just . top 1
      }
    , solveB = defSolver
      {
        {- | Part B Solver
          Repeat Part A, but this time for the top three elves.
        -}
        solve = Just . top 3
        -- Alternative point-ful version:
        -- solve xs = Just <| sum <| top 3 xs
      }
    , tests =
      [
        unlines [
            "1000"
          , "2000"
          , "3000"
          , ""
          , "4000"
          , ""
          , "5000"
          , "6000"
          , ""
          , "7000"
          , "8000"
          , "9000"
          , ""
          , "10000"
        ] :=> [(PartA, "24000"), (PartB, "45000")]
      ]
  }
  where
    numberLines = decimal `sepEndBy1` eol     -- list of numbers optionally separated by CRLF
    elfCalories = numberLines `sepEndBy1` eol -- list of ^ optionally separated by CRLF
    top 1 = maximum . map sum
    top n = sum . take n . sortBy descending . map sum
