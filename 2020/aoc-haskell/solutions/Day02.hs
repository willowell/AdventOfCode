module Day02 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Util

solution :: Solution
{- Parser Output Type -} [Password]
{- Part A Output Type -} Int
{- Part B Output Type -} Int
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = pPassword `sepEndBy1` eol <* eof
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
        solve = undefined
      }
    , tests =
      [
        unlines 
          [
            "1-3 a: abcde"
          , "1-3 b: cdefg"
          , "2-9 c: ccccccccc"
          ] :=> [(PartA, "2"), (PartB, "1")]
      ]
  }

data Password = Password
  {
    low :: Int
  , high :: Int
  , targetLetter :: Char
  , password :: String
  }

pPassword :: Parser Password
pPassword = do
  low <- decimal <?> "first number"
  void (char '-')
  high <- decimal <?> "second number"
  void space1
  targetLetter <- lowerChar <?> "target letter"
  void (char ':')
  void space1
  password <- some lowerChar
  return Password {..}
