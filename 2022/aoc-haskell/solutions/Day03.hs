module Day03 where

import AOC.Solution
import ParsingPrelude
import Data.Char
import Data.List
import Data.List.Split
import Util

{- | Day 3: Rucksack Reorganization

AKA an unsolicited advertisement for AirTags!

-}

solution :: Solution
{- Parser Output Type -} [String]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = word `sepEndBy1` eol
    , solveA = defSolver
      {
        {- | Part A Solver
        In Part A, we need to know which item exists in both of the two compartments in each rucksack,
        and then sum up these items' priorities.
        Since each string represents a rucksack, all we need to do is split it in half,
        take the intersection of the two halves, and then do the number crunching.
        -}
        solve = Just <$> sum' . map toPriority <$> head <$> (\(x, y) -> x `intersect` y) <$> splitHalf
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        Where Part A focused on single rucksacks, now we are taking the intersection of three rucksacks.
        -}
        solve = Just <$> sum' . map toPriority <$> head <$> (foldr1 intersect) . chunksOf 3
      }
    , tests =
      [
        unlines [
          "vJrwpWtwJgWrhcsFMMfFFhFp",
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
          "PmmdzqPrVvPwwTWBwg",
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
          "ttgJtRGJQctTZtZT",
          "CrZsJsPPZsGzwwsLwLmpwMDw"
        ] :=> [(PartA, "157"), (PartB, "70")]
      ]
  }

-- The letters are in the range 'a'..'z', 'A'..'Z',
-- so we can use a little modular arithmetic to contrain the value.
toPriority :: Char -> Int
toPriority c = (ord c - 96) `mod` 58

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt half xs
  where half = length xs `div` 2
