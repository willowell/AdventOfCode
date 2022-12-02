module Day01 where

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
        Parse the input into a list of integers
      -}
      decodeInput = decimal `sepEndBy1` eol
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = \xs -> Just $ head $
          [
              a * b
            | a <- xs,
              b <- xs,
              (a + b) == 2020
          ]
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = \xs -> Just $ head $
          [
              a * b * c
            | a <- xs,
              b <- xs,
              c <- xs,
              (a + b + c) == 2020
          ]
      }
    , tests =
      [
        unlines [
            "1721"
          , "979"
          , "366"
          , "299"
          , "675"
          , "1456"
        ] :=> [(PartA, "514579"), (PartB, "241861950")]
      ]
  }
