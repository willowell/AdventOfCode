module Day06 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

import Debug.Trace

solution :: Solution
{- Parser Output Type -} String
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = word
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = Just .
          (\xs ->
            let firstSubstr = getFirstUniqueSubstringKLength 4 xs in
            traceShow firstSubstr
            (+ 4) $ getIndexOfFirstSubstring firstSubstr xs
          )
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = Just .
          (\xs ->
            let firstSubstr = getFirstUniqueSubstringKLength 14 xs in
            traceShow firstSubstr
            (+ 14) $ getIndexOfFirstSubstring firstSubstr xs
          )
      }
    , tests =
      [
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb"    :=> [(PartA, "7"), (PartB, "19")]
      , "bvwbjplbgvbhsrlpgdmjqwftvncz"      :=> [(PartA, "5"), (PartB, "23")]
      , "nppdvjthqldpwncqszvftbrmjlhg"      :=> [(PartA, "6"), (PartB, "23")]
      , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" :=> [(PartA, "10"), (PartB, "29")]
      , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  :=> [(PartA, "11"), (PartB, "26")]
      ]
  }
