module Day04 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

{- | Day 4: Camp Cleanup

-}

type Assignment = [Int]

solution :: Solution
{- Parser Output Type -} [(Assignment, Assignment)]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      Finally, a non-trivial parser!
      Each line corresponds to a pair of elves, separated by commas.
      Each pair of numbers (e.g., 2-4) represents that elf's assignment.
      -}
      decodeInput = pAssignmentPair `sepEndBy1` eol
    , solveA = defSolver
      {
        {- | Part A Solver
        Wouldn't you know it, Haskell has an `isInfixOf` function on lists that does exactly what we need:
        check if a list wholy contains a sublist.
        So, all we need to do is check it both ways.
        -}
        solve =
          Just
          . length
          . filter (\(f, s) -> eitherWholyContains f s)
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        Again, `intersect` works fine here - we just need to know if the result of `intersect` is non-empty.
        -}
        solve =
          Just
          . length
          . filter (\(f, s) -> f `overlaps` s)
      }
    , tests =
      [
        unlines [
          "2-4,6-8",
          "2-3,4-5",
          "5-7,7-9", -- these two overlap at 7.
          "2-8,3-7", -- the first elf's assignment wholy contains the second's.
          "6-6,4-6",
          "2-6,4-8"
        ] :=> [(PartA, "2"), (PartB, "4")]
      ]
  }
  where
    pAssignment :: Parser Assignment
    pAssignment = do
      start <- decimal
      void $ char '-'
      end <- decimal
      return [start..end]
    pAssignmentPair :: Parser (Assignment, Assignment)
    pAssignmentPair = do
      first <- pAssignment
      void $ char ','
      second <- pAssignment
      return (first, second)
    eitherWholyContains xs ys =
      xs `isInfixOf` ys || ys `isInfixOf` xs
    overlaps xs ys =
      not . null $ (intersect xs ys)
