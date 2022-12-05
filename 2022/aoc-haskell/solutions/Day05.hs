module Day05 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Data.Maybe
import Util

type Instruction = (Int, Int, Int)

data Procedure = Procedure { stacks :: [[Char]], instructions :: [Instruction] }

instance Show Procedure where
  show a = (show $ stacks a) <> (show $ instructions a)

solution :: Solution
{- Parser Output Type -} Procedure
{- Part A Output Type -} Procedure
{- Part B Output Type -} [[Char]]
solution = Solution
  {
      {- | Input Parser
      -}
      decodeInput = pProcedure
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = Just . id
      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = pure Nothing
      }
    , tests =
      [
        unlines [
            "    [D]    "
          , "[N] [C]    "
          , "[Z] [M] [P]"
          , " 1   2   3 "
          , ""
          , "move 1 from 2 to 1"
          , "move 3 from 1 to 3"
          , "move 2 from 2 to 1"
          , "move 1 from 1 to 2"
        ] :=> [(PartA, "CMZ"), (PartB, "")]
      ]
  }
  where
    pCrate :: Parser (Maybe Char)
    pCrate = choice
      [
        Nothing <$ between (char ' ') (char ' ') (char ' ')
      , Just <$> between (char '[') (char ']') letterChar
      ]

    pStack :: Parser [Maybe Char]
    pStack = pCrate `sepBy` (char ' ')

    pNumberLabels :: Parser String
    pNumberLabels = some (digitChar <|> char ' ') <* eol

    pInstruction :: Parser Instruction
    pInstruction = do
      void $ string "move"
      target <- between space1 space1 decimal
      void $ string "from"
      from <- between space1 space1 decimal
      void $ string "to"
      to <- space1 *> decimal
      return (target, from, to)

    pInstructions :: Parser [Instruction]
    pInstructions = pInstruction `sepEndBy1` eol

    pProcedure :: Parser Procedure
    pProcedure = do
      maybeStacks <- dbg "stacks" (someTill (dbg "stack" pStack <* eol) pNumberLabels) <?> "stacks"

      let stacks = map catMaybes $ transpose . reverse $ maybeStacks

      void eol <?> "empty line"

      instructions <- pInstructions <?> "instructions"

      return Procedure{..}
