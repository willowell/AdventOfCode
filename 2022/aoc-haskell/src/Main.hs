module Main where

import AOC.Harness
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04

solutions :: Solutions
solutions = solutionsFromList
  [
    S Day01.solution
  , S Day02.solution
  , S Day03.solution
  , S Day04.solution
  ]

main :: IO ()
main = aocMain 2022 solutions

