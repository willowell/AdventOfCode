module Day02 where

import AOC.Solution
import ParsingPrelude
import Data.Finite
import Data.List
import Util

{- | Day 2: Rock Paper Scissors

-}

type Move = Finite 3

type Round = (Move, Move)

getScore :: (Round -> Move) -> (Round -> Move) -> Round -> Integer
getScore getShapeScore getOutcomeScore rnd
  = getFinite (getShapeScore rnd) + 1 + getFinite (getOutcomeScore rnd) * 3

solution :: Solution
{- Parser Output Type -} [Round]
{- Part A Output Type -} Integer
{- Part B Output Type -} Integer
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = pRound `sepEndBy1` eol
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve =
          Just
          . sum
          . map (getScore snd (\(x, y) -> y + (1 - x)))
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve =
          Just
          . sum
          . map (getScore (\(x, y) -> y - (1 - x)) snd)
      }
    , tests = ["A Y\nB X\nC Z" :=> [(PartA, "15"), (PartB, "12")]]
  }
  where
    pMove :: Parser Move
    pMove = choice
      [
          0 <$ (char 'A' <|> char 'X') <?> "rock (A/X)"
        , 1 <$ (char 'B' <|> char 'Y') <?> "paper (B/Y)"
        , 2 <$ (char 'C' <|> char 'Z') <?> "scissors (C/Z)"
      ]
    pRound :: Parser Round
    pRound = do
      your <- pMove
      void space1
      my <- pMove
      return (your, my)
