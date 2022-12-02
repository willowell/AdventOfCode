module Day00 where

{-
This file is not meant to be run on its own!

Please use it as a template.
-}

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
      decodeInput = undefined
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = undefined
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = undefined
      }
    , tests =
      [
        "" :=> [(PartA, ""), (PartB, "")]
      ]
  }
