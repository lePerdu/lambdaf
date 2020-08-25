{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Brainfuck DSL for working with StackIr pseudo cells
module LambdaF.StackIr.StackBf
  ( StackCell (..),
    ScanDirection (..),
    StackBfM,
    stackCellSize,
    stackCellIndex,
    firstStackCell,
    oppositeScanDir,
    stackBfIntoBf,
    stackBfFromBf,
    stackDoAt,
    moveStackCell,
    movePseudoCell,
    clearCell,
    incrementTag,
    decrementTag,
    incrementValue,
    incrementWork,
    markCell,
    unmarkCell,
    clearPseudoCell,
    scanAndDo,
    scanAt,
    scanSeparator,
    scanMark,
    stackInput,
    stackOutput,
    stackLoop,
    stackCond,
    copyCell,
  )
where

import Control.Monad.State
import Lens.Micro.Platform
import Pinky.Brainfuck.Monad

data StackCell
  = ValueCell
  | TagCell
  | MarkCell
  | WorkCell

stackCellIndex :: StackCell -> Int
stackCellIndex ValueCell = 3
stackCellIndex TagCell = 2
stackCellIndex MarkCell = 1
stackCellIndex WorkCell = 0

stackCellSize :: Int
stackCellSize = 4

firstStackCell :: StackCell
firstStackCell = WorkCell

data ScanDirection = ScanLeft | ScanRight

oppositeScanDir :: ScanDirection -> ScanDirection
oppositeScanDir ScanLeft = ScanRight
oppositeScanDir ScanRight = ScanLeft

data StackState = StackState {_stackCell :: !StackCell}

makeLenses ''StackState

newtype StackBfM a = StackBfM {unStackBf :: StateT StackState BrainfuckM a}
  deriving (Functor, Applicative, Monad)

stackBfIntoBf :: StackCell -> StackBfM a -> BrainfuckM a
stackBfIntoBf startingCell stackBf =
  evalStateT (unStackBf stackBf) (StackState startingCell)

-- | Run "raw" Brainfuck in StackBfM
--
-- The raw code should satisfy
stackBfFromBf :: BrainfuckM a -> StackBfM a
stackBfFromBf = StackBfM . lift

stackDoAt :: StackCell -> BrainfuckM a -> StackBfM a
stackDoAt cell action = do
  moveStackCell cell
  stackBfFromBf action

setZero :: BrainfuckM ()
setZero = loop decrement

movePseudoCell :: Integral a => a -> StackBfM ()
movePseudoCell n = stackBfFromBf $ moveN (fromIntegral n * stackCellSize)

moveStackCell :: StackCell -> StackBfM ()
moveStackCell target = StackBfM $ do
  current <- gets _stackCell
  lift $ moveN (stackCellIndex target - stackCellIndex current)
  stackCell .= target

clearCell :: StackCell -> StackBfM ()
clearCell cell = stackDoAt cell setZero

incrementTag :: StackBfM ()
incrementTag = stackDoAt TagCell increment

decrementTag :: StackBfM ()
decrementTag = stackDoAt TagCell decrement

incrementValue :: Integral a => a -> StackBfM ()
incrementValue n = stackDoAt ValueCell (incrementN n)

incrementWork :: Integral a => a -> StackBfM ()
incrementWork n = stackDoAt WorkCell (incrementN n)

markCell :: StackBfM ()
markCell = clearCell MarkCell

unmarkCell :: StackBfM ()
unmarkCell = stackDoAt MarkCell $ do
  -- Don't assume already marked
  setZero
  increment

clearPseudoCell :: StackBfM ()
clearPseudoCell = do
  clearCell ValueCell
  clearCell TagCell
  -- clearCell WorkCell -- Don't need to do this
  unmarkCell

scanDirMove :: ScanDirection -> BrainfuckM ()
scanDirMove ScanLeft = moveN (- stackCellSize)
scanDirMove ScanRight = moveN stackCellSize

-- | Scan for a 0 cell, performing an action at each cell
--
-- The scan does not check or run the action on the current pseudo cell.
-- The action is expected to stay within the same pseudo cell.
scanAndDo :: StackCell -> ScanDirection -> StackBfM () -> StackBfM ()
scanAndDo cell dir action = stackDoAt cell $ do
  moveDir
  loop $ do
    stackBfIntoBf cell $ do
      action
      -- Assure it ends on the same cell it started on
      moveStackCell cell
    moveDir
  where
    moveDir = scanDirMove dir

scanAt :: StackCell -> ScanDirection -> StackBfM ()
scanAt cell dir = scanAndDo cell dir (pure ())

scanSeparator :: ScanDirection -> StackBfM ()
scanSeparator = scanAt TagCell

scanMark :: ScanDirection -> StackBfM ()
scanMark = scanAt MarkCell

stackInput :: StackBfM ()
stackInput = stackDoAt ValueCell input

stackOutput :: StackBfM ()
stackOutput = stackDoAt ValueCell output

stackLoop :: StackCell -> StackBfM () -> StackBfM ()
stackLoop cell action = stackDoAt cell $ loop (stackBfIntoBf cell action')
  where
    -- Make sure the loop ends on the same stack cell (though not necessarily
    -- the same pseudo cell)
    action' = action >> moveStackCell cell

stackCond :: StackCell -> StackBfM () -> StackBfM ()
stackCond cell action = stackLoop cell (action >> clearCell cell)

-- | Destructive copy into a new cell
copyCell :: StackCell -> StackCell -> StackBfM ()
copyCell src dst = stackLoop src $ do
  stackDoAt src decrement
  stackDoAt dst increment
