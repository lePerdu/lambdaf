{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaF.StackIr.Compiler
  ( StackIrError (..),
    IndexedIr (..),
    IrReader,
    irInstrToBf,
    irFunctionToBf,
    stackIrToBf,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.Text as T
import LambdaF.StackIr.Language
import LambdaF.StackIr.StackBf
import Lens.Micro.Platform
import Pinky.Brainfuck

data IndexedIr = IndexedIr
  {_iIrFunctions :: M.HashMap T.Text (Int, [StackIrInstr])}
  deriving (Show)

makeLenses ''IndexedIr

data StackIrError = FunctionNotFound !T.Text
  deriving (Show)

-- TODO Make this a transformer on StackBfM?
type IrReader = ReaderT IndexedIr (Except StackIrError)

-- | Make a value in the current cell, possibly destroying any later cells.
setValueCell :: Int -> StackBfM ()
setValueCell n = clearCell ValueCell >> incrementValue n

setValueCellIr :: StackValue -> IrReader (StackBfM ())
-- TODO Better error handling if the identifier doesn't exist
setValueCellIr (StackIdent ident) = do
  funcs <- asks _iIrFunctions
  index <- case M.lookup ident funcs of
    Just (index, _) -> pure index
    Nothing -> throwError $ FunctionNotFound ident
  pure $ setValueCell index
setValueCellIr (StackLiteral n) = pure $ setValueCell n

makeNewValueCell :: StackBfM () -> StackBfM ()
makeNewValueCell setter = do
  -- Separator
  movePseudoCell 1
  clearPseudoCell

  movePseudoCell 1
  clearPseudoCell
  incrementTag
  setter

longCopy :: ScanDirection -> StackBfM ()
longCopy dir = do
  -- Copy the value (preserving via work cell)
  copyCell ValueCell WorkCell
  stackLoop WorkCell $ do
    -- Back into the value cell
    incrementWork (-1)
    incrementValue 1

    -- Into the target value cell
    scanMark dir
    incrementValue 1
    scanMark opposite

  -- Copy tag (preserving via work cell)
  copyCell TagCell WorkCell
  stackLoop WorkCell $ do
    -- Back into the value cell
    incrementWork (-1)
    incrementTag

    -- Into the target value cell
    scanMark dir
    incrementTag
    scanMark opposite
  where
    opposite = oppositeScanDir dir

irInstrToBf :: StackIrInstr -> IrReader (StackBfM ())
irInstrToBf (StackPush v) = makeNewValueCell <$> setValueCellIr v
irInstrToBf (StackDup n) = pure $ do
  -- Make a new separator
  movePseudoCell 1
  clearPseudoCell

  -- Create and mark target
  movePseudoCell 1
  clearPseudoCell
  markCell

  -- Scan to the source
  movePseudoCell (-1)
  replicateM_ n (scanSeparator ScanLeft)

  -- Mark the source
  movePseudoCell 1
  markCell

  -- Copy cells until we reach a separator
  stackLoop TagCell $ do
    longCopy ScanRight

    -- Shift target mark
    scanMark ScanRight
    unmarkCell
    movePseudoCell 1
    clearPseudoCell
    markCell
    scanMark ScanLeft

    -- Shift source mark
    unmarkCell
    movePseudoCell 1
    markCell

  -- Unmark the source separator cell
  unmarkCell

  -- Unmark the target
  scanMark ScanRight
  unmarkCell
  movePseudoCell (-1)
irInstrToBf (StackDel 1) = pure $ do
  scanSeparator ScanLeft
  movePseudoCell (-1)
irInstrToBf (StackDel n) = pure $ do
  -- Put mark to know when to stop copying
  movePseudoCell 1
  markCell

  -- Scan to the target, clear it, and mark it
  replicateM_ n (scanSeparator ScanLeft)
  movePseudoCell 1
  clearPseudoCell
  markCell

  -- Go to the one to copy
  scanSeparator ScanRight
  movePseudoCell 1

  -- Copy cells until we reach the mark (of 0)
  stackLoop MarkCell $ do
    -- Mark the source cell to copy
    markCell

    -- TODO Don't need to preserve the original value
    longCopy ScanLeft

    -- Shift target mark
    scanMark ScanLeft
    unmarkCell
    movePseudoCell 1
    clearPseudoCell
    markCell
    scanMark ScanRight

    -- Shift source cell (but don't mark, because that's done at the start)
    unmarkCell
    movePseudoCell 1

  -- Unmark the end of the stack
  unmarkCell

  -- Scan to where the top element was copied
  scanMark ScanLeft
  unmarkCell

  movePseudoCell (-1)
irInstrToBf StackInput = pure $ makeNewValueCell stackInput
irInstrToBf StackOutput = pure $ do
  stackOutput
  movePseudoCell (-2)
irInstrToBf (StackCond ifTrue ifFalse) = do
  -- TODO If the values are close (maybe even if not), make one and then
  -- have a branch to modify it to be the other so that we don't need 2 flags
  makeIfTrue <- setValueCellIr ifTrue
  makeIfFalse <- setValueCellIr ifFalse
  pure $ do
    -- Set 2 flags which are inverted if the value is non-0
    -- (using the tag as an extra flag)
    --
    -- If false,
    -- tag = 1, work = 0
    unmarkCell
    clearCell WorkCell

    let condTrue = markCell >> incrementWork 1

    -- If tag > 0, the top element is a tuple
    decrementTag
    stackCond TagCell $ condTrue

    -- If value /= 0
    stackCond ValueCell $ condTrue

    let popStack = scanSeparator ScanLeft >> movePseudoCell 1

    -- This relies on exactly one of the following running.
    -- Each check removes the top element from the stack and creates a new one

    stackCond MarkCell $ do
      popStack
      makeIfFalse

      -- This is important so that the next condition does not also run
      clearCell WorkCell

    stackCond WorkCell $ do
      popStack
      makeIfTrue

    -- Finish cell setup
    clearCell TagCell
    incrementTag
    unmarkCell
irInstrToBf (StackPack n) = pure $ do
  -- TODO Clear the tag instead of marking?
  -- Always ensure the top of the stack is a separator cell?
  movePseudoCell 1
  markCell

  replicateM_ (n - 1) $ do
    scanAndDo TagCell ScanLeft incrementTag
    incrementTag

  -- For the last one, leave the final separator
  scanAndDo TagCell ScanLeft incrementTag

  -- Back to the top
  scanMark ScanRight
  unmarkCell
  movePseudoCell (-1)
irInstrToBf StackUnpack = pure $ do
  markCell
  movePseudoCell 1

  scanAndDo TagCell ScanLeft decrementTag

  scanMark ScanRight
  unmarkCell
irInstrToBf (StackPrim (StackIrPrim _ code)) = pure $ code

markIfNonZero :: StackBfM ()
markIfNonZero = do
  -- Copy into work cell
  clearCell WorkCell
  copyCell ValueCell WorkCell

  stackCond WorkCell $ do
    markCell
    -- Copy back into ValueCell (if the condition isn't met, it was 0 anyway)
    copyCell WorkCell ValueCell

-- | Compile function internal code into CollapsedBf
irFunctionToBf :: Int -> [StackIrInstr] -> IrReader (StackBfM ())
irFunctionToBf index code = do
  innerCode <- sequence_ <$> traverse irInstrToBf code
  pure $ do
    -- Decrement address
    incrementValue (-1)

    markIfNonZero -- Unmarked by default
    stackLoop MarkCell $ do
      -- Pop the function value off the stack
      movePseudoCell (-1)

      -- Unpack until the top of the stack is a raw value
      -- Check the separator after the function index to know when it's not a
      -- tuple
      markCell
      stackLoop TagCell $ do
        -- Adjust forward to scan backward
        movePseudoCell 1
        scanAndDo TagCell ScanLeft decrementTag
        scanMark ScanRight

      -- Move over the separator to the new top of the stack
      unmarkCell
      movePseudoCell (-1)

      -- Actual code
      innerCode

      -- Decrement address (last cell in the stack) by current index so
      -- that the index is relative
      incrementValue (- index)
      -- If the index is the current one (i.e. is now 0), continue the
      -- loop
      markIfNonZero

    -- If the function wasn't run
    unmarkCell

makeFunctionIndices :: StackIr -> IndexedIr
makeFunctionIndices (StackIr functions) =
  let combineIndex (StackIrFunc name code) index = (name, (index, code))
      combinedList = zipWith combineIndex functions [1 ..]
   in IndexedIr (M.fromList combinedList)

stackIrToBf :: StackIr -> Either StackIrError Bf
stackIrToBf stackIr =
  let indexedIr@(IndexedIr functions) = makeFunctionIndices stackIr
   in intoBf . stackBfIntoBf firstStackCell
        <$> runExcept
          ( flip runReaderT indexedIr $ do
              initializeValue <- setValueCellIr (StackIdent "main")
              let functionCount = M.size functions
              functionsBf <-
                sequence_ <$> traverse (uncurry irFunctionToBf) functions
              pure $ do
                -- Initialize with a separator cell and the main function
                clearPseudoCell
                movePseudoCell 1
                clearPseudoCell
                movePseudoCell 1
                clearPseudoCell
                incrementTag
                initializeValue
                stackLoop ValueCell $ do
                  functionsBf
                  -- Wrap the function index back around to restart the loop
                  incrementValue functionCount
          )
