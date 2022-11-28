module AOC.Solution where

import Data.Typeable

import DynMap
import ParsingPrelude (Parser)

data Part = PartA | PartB deriving (Eq, Ord, Show)

allParts :: [Part]
allParts = [PartA, PartB]

displayPart :: Part -> Char
displayPart PartA = 'a'
displayPart PartB = 'b'

{- | Solution

Solution Datatype, taking three types:
* `i` - the type of the input, e.g., `String`
* `a` - the type of the output for Part A, e.g., `Int`
* `b` - the type of the output for Part B, e.g., `Boolean`

__Examples:__

@
solution :: Solution {- Input Type -} String {- Part A Output Type -} Int {- Part B Output Type -} Int
solution = Solution
  {
      -- Parser for the input
      decodeInput = string "foo"
    , solveA = defSolver
      {
        -- Part A solver
        solve = Just . length
      }
    , solveB = defSolver
      {
        -- Part B solver
        solve = Just . (length . nub)
      }
    , tests = [ "foo" :=> [(PartA, "3"), (PartB, "2")] ]
  }
@

-}
data Solution
  i -- ^ Input
  a -- ^ Part A Output
  b -- ^ Part B Output
  = Solution
  { decodeInput :: Parser i -- ^ Input parser.
  , solveA      :: Solver i a -- ^ Solver for Part A. 
  , solveB      :: Solver i b -- ^ Solver for Part B.
  , tests       :: [Test]
  }

data Solver i o = Solver
  { solve :: HasDyns => i -> Maybe o
  , display :: HasDyns => o -> String
  }

defSolver :: Show o => Solver i o
defSolver = Solver (const Nothing) show

runSolver :: HasDyns => Solution i a b -> Part -> i -> Maybe String
runSolver Solution{solveA = Solver{solve,display}} PartA dat = display <$> solve dat
runSolver Solution{solveB = Solver{solve,display}} PartB dat = display <$> solve dat

data Test
  = String :=> [(Part, String)]
  | forall a. Typeable a => WithDyn String a Test

processTest :: Test -> (DynMap, String, [(Part, String)])
processTest (i :=> o) = (emptyDynMap, i, o)
processTest (WithDyn k v inner) = (addDyn k v dyns, i, o)
  where (dyns, i, o) = processTest inner