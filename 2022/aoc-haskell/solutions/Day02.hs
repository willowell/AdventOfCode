{-# LANGUAGE RecordWildCards #-}
module Day02 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

data Action = Rock | Paper | Scissors

type Lose = Rock
type Draw = Paper
type Win = Scissors

getActionScore Rock = 1
getActionScore Paper = 2
getActionScore Scissors = 3

data Move = Move { your :: Action, my :: Action }

getMoveScore Move {your = Rock, my = Paper}    = 6
getMoveScore Move {your = Paper, my = Scissors}    = 6
getMoveScore Move {your = Scissors, my = Rock}    = 6
getMoveScore Move {your = Rock, my = Rock}    = 3
getMoveScore Move {your = Paper, my = Paper}    = 3
getMoveScore Move {your = Scissors, my = Scissors}    = 3
getMoveScore Move {your = _, my = _}    = 0

getMoveScore' Move {your = a, my = Paper}    = Move {your = a, my = a}
getMoveScore' Move {your = a, my = Rock} = getLosingMove a
getMoveScore' Move {your = a, my = Scissors} = getWinningMove a

getLosingMove Rock = Move {your = Rock, my = Scissors}
getLosingMove Paper = Move {your = Paper, my = Rock}
getLosingMove Scissors = Move {your = Scissors, my = Paper}

getWinningMove Rock = Move {your = Rock, my = Paper}
getWinningMove Paper = Move {your = Paper, my = Scissors}
getWinningMove Scissors = Move {your = Scissors, my = Rock}

getRoundScore mv =
  let roundScore = getMoveScore mv in
    roundScore + (getActionScore $ my mv)

getRoundScore' mv =
  let newMove = getMoveScore' mv in
  let roundScore = getMoveScore newMove in
    roundScore + (getActionScore $ my newMove)

solution :: Solution
{- Parser Output Type -} [Move]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = pMove `sepEndBy1` eol


    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = Just . sum . map getRoundScore

      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = Just . sum . map getRoundScore'
      }
    , tests =
      [
        "A Y\nB X\nC Z" :=> [(PartA, "15"), (PartB, "12")]
      ]
  }
  where
    pAction = choice
      [
          Rock     <$ (char 'A' <|> char 'X') <?> "rock (A/X)"
        , Paper    <$ (char 'B' <|> char 'Y') <?> "paper (B/Y)"
        , Scissors <$ (char 'C' <|> char 'Z') <?> "scissors (C/Z)"
      ]
    pMove = do
      your <- pAction
      void space1
      my <- pAction
      return Move{..}

