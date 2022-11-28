module Day01 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

solution :: Solution {- Input Type -} String {- Part A Output Type -} Int {- Part B Output Type -} Int
solution = Solution
  {
      -- Parser for the input
      decodeInput = string "foo"
    , solveA = defSolver
      {
        -- Part A solver
        solve = Just . length
      }
    , solveB = defSolver
      {
        -- Part B solver
        solve = Just . (length . nub)
      }
    , tests =
      [
        "foo" :=> [(PartA, "3"), (PartB, "2")]
      ]
  }
