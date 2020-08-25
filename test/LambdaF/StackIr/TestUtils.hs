module LambdaF.StackIr.TestUtils where

import Control.Monad.Identity
import Data.Foldable
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Pinky.Brainfuck
import LambdaF.StackIr.StackBf
import Test.QuickCheck

data PseudoCell c = PseudoCell
  { _cellValue :: !c,
    _cellTag :: !c,
    _cellMark :: !c,
    _cellWork :: !c
  }
  deriving (Show)

-- | Compare stack elements, ignoring the work cell
instance Eq c => Eq (PseudoCell c) where
  a == b =
    on (==) _cellValue a b
      && on (==) _cellTag a b
      && on (==) _cellMark a b

data StackElem c = StackScalar c | StackTuple (NonEmpty (StackElem c))
  deriving (Show, Eq)

instance (Integral c, Arbitrary c) => Arbitrary (StackElem c) where
  arbitrary = sized arb
    where
      scalar = StackScalar <$> arbitrary
      arb 0 = StackScalar <$> arbitrary
      arb n =
        frequency
          [ (2, scalar),
            (1, arbTuple (n - 1))
          ]
      arbTuple n = do
        Positive m <- arbitrary
        let subElem = arb (n `div` m)
        headElem <- subElem
        elems <- vectorOf 1 subElem
        pure $ StackTuple (headElem :| elems)

makeSeparator :: Integral c => c -> PseudoCell c
makeSeparator depth = PseudoCell 0 depth 1 0

makeElem :: Integral c => c -> StackElem c -> [PseudoCell c]
makeElem depth (StackScalar v) = [PseudoCell v (depth + 1) 1 0]
makeElem depth (StackTuple vs) = makeTuple (depth + 1) (toList vs)

makeTuple :: Integral c => c -> [StackElem c] -> [PseudoCell c]
makeTuple depth = intercalate [makeSeparator depth] . map (makeElem depth)

-- | Make a full stack of elements
--
-- This is just a tuple of depth 0, with an extra separator cell appended.
makeStack :: Integral c => [StackElem c] -> [PseudoCell c]
makeStack [] = [makeSeparator 0]
makeStack elems = makeTuple 0 elems ++ [sep, sep]
  where
    sep = makeSeparator 0

-- | Builds a BrainfuckTape action to initialize the contents from pseudo cells
--
-- Pseudo cells are written with the first element in the list being the top of
-- the stack. This is somewhat counter-intuitive since the list head is on the
-- left in haskell but the top of the Brainfuck stack is on the right.
--
-- Leaves the tape head on the top pseudo cell of the stack (where it's
-- expected for StackIr instructions).
makeInitTape :: Monad m => BfCell c => [PseudoCell c] -> BrainfuckTape c m ()
makeInitTape cells = do
  traverse_ makePseudoCell (reverse cells)
  moveHead (stackCellIndex ValueCell - stackCellSize)
  where
    makePseudoCell pseudoCell = do
      set ValueCell _cellValue
      set TagCell _cellTag
      set MarkCell _cellMark
      set WorkCell _cellWork
      moveHead stackCellSize
      where
        set cell f = setCellOffset (stackCellIndex cell) (f pseudoCell)

-- | Fully truncate the right side of the tape
--
-- This is used because the tape after the stack is always ignored
truncateStack :: BfState c -> [c]
truncateStack tape = _bfHead tape : _bfTapeLeft tape

-- | Make and truncate a Brainfuck tape from pseudo cells
makeTape :: BfCell c => [PseudoCell c] -> [c]
makeTape cells =
  let (_, tape) = runIdentity $ runBfTape $ makeInitTape cells
   in truncateStack tape

makeStackTape :: BfCell c => [StackElem c] -> [c]
makeStackTape = makeTape . makeStack
