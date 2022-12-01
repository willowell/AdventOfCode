module Day03 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

solution :: Solution
{- Parser Output Type -} [Int]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = decimal `sepEndBy1` eol <* eof
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = Just . minimum
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = Just . maximum
      }
    , tests =
      [
        "1000\n2000" :=> [(PartA, "1000"), (PartB, "2000")]
      ]
  }
