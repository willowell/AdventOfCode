module Day05 where

import AOC.Solution
import ParsingPrelude
import Data.List
import Data.Maybe
import Util

type Crate = Char

type Stack = (Int, [Crate])

type Instruction = (Int, Int, Int)

data Procedure = Procedure
  {
    stacks :: [Stack]
  , instructions :: [Instruction]
  }

replaceStack :: [Stack] -> Int -> [Crate] -> [Stack]
replaceStack stacks stack newStack = map (\s@(i, _) -> if i == stack then (i, newStack) else s) stacks

rearrange :: [Stack] -> Int -> Int -> Int -> Bool -> [Stack]
rearrange stacks numCrates from to reversePulled =
  let fromIndex = from - 1 in
  let toIndex = to - 1 in

  let fromStack = stacks !! fromIndex in
  let toStack = stacks !! toIndex in

  let movedCrates = take numCrates $ snd fromStack in

  let oldStack = drop numCrates $ snd fromStack in

  let newStack = (if reversePulled then reverse else id) movedCrates <> snd toStack in

  let s = replaceStack stacks from oldStack in

  replaceStack s to newStack

getTopCrates :: [Stack] -> [Crate]
getTopCrates stacks = map head $ map snd stacks

runInstructions :: Bool -> Procedure -> [Stack]
runInstructions reversePulled p =
  let ins = instructions p in
  let startingStacks = stacks p in
    foldl'
    (\curStacks (numCrates, from, to) -> rearrange curStacks numCrates from to reversePulled)
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
        solve = Just <$> getTopCrates . (runInstructions True)

      }
    , solveB = defSolver
      {
        {- | Part B Solver
        -}
        solve = Just <$> getTopCrates . (runInstructions False)
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
        ] :=> [(PartA, "\"CMZ\""), (PartB, "\"MCD\"")]
      ]
  }
  where
    pCrate :: Parser (Maybe Crate)
    pCrate = choice
      [
        Nothing <$ between (char ' ') (char ' ') (char ' ')
      , Just <$> between (char '[') (char ']') letterChar
      ]

    pStack :: Parser [Maybe Crate]
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

      let stacks = zip [1..] $ map reverse $ map catMaybes $ transpose . reverse $ maybeStacks

      void eol <?> "empty line"

      instructions <- pInstructions <?> "instructions"

      return Procedure{..}
