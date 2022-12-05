module Day05 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Data.Maybe
import Util

import qualified Data.Vector.Mutable  as MVec

type Instruction = (Int, Int, Int)

data Procedure = Procedure { stacks :: [[Char]], instructions :: [Instruction] }

prettyPrintProcedure p =
  "Stacks: `" <> (show $ stacks p) <> "`\nInstructions: `" <> (show $ instructions p) <> "`."

replaceStack :: [[Char]] -> [Char] -> [Char] -> [[Char]]
replaceStack stacks stack newStack = map (\s -> if s == stack then newStack else s) stacks

rearrange :: [[Char]] -> Int -> Int -> Int -> [[Char]]
rearrange stacks numCrates from to =
  let fromStack = stacks !! (from - 1) in
  let toStack = stacks !! (to - 1) in

  let movedCrates = take numCrates fromStack in

  let oldStack = drop numCrates fromStack in

  let newStack = reverse movedCrates <> toStack in

  let stacksWithReplacedTo = replaceStack stacks toStack newStack in
    replaceStack stacksWithReplacedTo fromStack oldStack

getTopCrates :: [[Char]] -> [Char]
getTopCrates stacks = map head stacks

runInstructions :: Procedure -> [[Char]]
runInstructions p =
  let ins = instructions p in
  let startingStacks = stacks p in
    foldl
    (\curStacks (numCrates, from, to) -> rearrange curStacks numCrates from to )
    startingStacks
    ins

runInstructions' :: Foldable t => [[Char]] -> t (Int, Int, Int) -> [[Char]]
runInstructions' stacks instructions =
  let ins = instructions in
  let startingStacks = stacks in
    foldl
    (\curStacks (numCrates, from, to) -> rearrange curStacks numCrates from to)
    startingStacks
    ins


solution :: Solution
{- Parser Output Type -} Procedure
{- Part A Output Type -} String
{- Part B Output Type -} String
solution = Solution
  {
      {- | Input Parser

      For the test input, returns stacks "ZN,MCD,P"
      
      For ease of use, we can reverse the stacks:
      1. NZ
      2. DCM
      3. P

      Next step:
      1. DNZ
      2. CM
      3. P

      Next step:
      1. <empty>
      2. CM
      3. ZNDP
      -}
      decodeInput = pProcedure
    , solveA = defSolver
      {
        {- | Part A Solver
        -}
        solve = Just <$> getTopCrates . runInstructions

      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = Just . prettyPrintProcedure
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
        ] :=> [(PartA, "\"CMZ\""), (PartB, "\"foo\"")]
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
      maybeStacks <- (someTill (pStack <* eol) pNumberLabels) <?> "stacks"

      let stacks = map reverse $ map catMaybes $ transpose . reverse $ maybeStacks

      void eol <?> "empty line"

      instructions <- pInstructions <?> "instructions"

      return Procedure{..}
