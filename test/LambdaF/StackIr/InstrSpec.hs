{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaF.StackIr.InstrSpec where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable
import qualified Data.HashMap.Strict as M
import Data.Function
import Data.List
import Data.List.NonEmpty (fromList)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Word
import LambdaF.StackIr.Compiler
import LambdaF.StackIr.Language
import LambdaF.StackIr.StackBf
import LambdaF.StackIr.TestUtils
import Pinky.Brainfuck
import Pinky.Brainfuck.Naive
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

instrsToStackBf ::
  [StackIrInstr] -> IndexedIr -> Either StackIrError (StackBfM ())
instrsToStackBf instrs state =
  fmap sequence_ $ runExcept $ runReaderT (traverse irInstrToBf instrs) state

runStackInstrs ::
  forall c.
  BfCell c =>
  IndexedIr ->
  [StackElem c] ->
  String ->
  [StackIrInstr] ->
  ([c], (String, String))
runStackInstrs state stack input instrs =
  let Right stackBf = instrsToStackBf instrs state
      -- Ensure it ends at the right cell
      stackBf' = stackBf >> moveStackCell ValueCell
      bf = intoBf $ stackBfIntoBf ValueCell stackBf'

      initialTapeAction = makeInitTape $ makeStack stack
      bfAction = initialTapeAction >> interpretBasicTape bf

      ((_, finalTape), io) = runBufferMachine (runBfTape bfAction) input
   in (truncateStack finalTape, io)

runStackInstr ::
  forall c.
  BfCell c =>
  IndexedIr ->
  [StackElem c] ->
  String ->
  StackIrInstr ->
  ([c], (String, String))
runStackInstr state stack input instr =
  runStackInstrs state stack input [instr]

emptyContext :: IndexedIr
emptyContext = IndexedIr mempty

-- TODO Make generic over cell type

checkInstrs ::
  [StackElem Word8] -> [StackElem Word8] -> [StackIrInstr] -> Property
checkInstrs initStack expectedStack instrs =
  let (tape, _) = runStackInstrs emptyContext initStack "" instrs
      expectedTape = makeStackTape expectedStack
   in (expectedTape :: [Word8]) === tape

checkInstr ::
  [StackElem Word8] -> [StackElem Word8] -> StackIrInstr -> Property
checkInstr initStack expectedStack instr =
  checkInstrs initStack expectedStack [instr]

spec :: Spec
spec = do
  describe "StackPush" $ do
    it "pushes literal" $
      property $
        \initStack value ->
          let instr = StackPush (StackLiteral (fromIntegral value))
              expectedStack = StackScalar value : initStack
           in checkInstr initStack expectedStack instr

    it "pushes function ref" $
      property $
        \initStack funcIndex funcName ->
          let state =
                IndexedIr (M.singleton funcName (fromIntegral funcIndex, []))
              instr = StackPush (StackIdent funcName)
              (tape, _) = runStackInstr state initStack "" instr
              expectedTape =
                makeStackTape (StackScalar funcIndex : initStack)
           in (expectedTape :: [Word8]) === tape

{-
  describe "StackInc" $ do
    it "increments" $
      property $
        \baseStack topValue incValue ->
          -- Ensure the top value is a scalar
          let initStack = StackScalar topValue : baseStack
              instr = StackInc (fromIntegral incValue)
              expectedStack = StackScalar (topValue + incValue) : baseStack
           in checkInstr initStack expectedStack instr
-}

  describe "StackPack" $ do
    it "packs into tuple" $
      property $
        \baseStack (Positive n) -> do
          topStack <- vector n
          let initStack = topStack ++ baseStack
          -- Always non-empty since n > 0
          let tuple = fromList topStack
          let expectedStack = StackTuple tuple : baseStack
          pure $
            counterexample ("Tuple: " ++ show topStack) $
              checkInstr initStack expectedStack (StackPack n)

  describe "StackUnpack" $ do
    it "unpacks from tuple" $
      property $
        \baseStack (Positive n) -> do
          tupleElems <- vector n
          -- Always non-empty since n > 0
          let tuple = fromList tupleElems
          let initStack = StackTuple tuple : baseStack
          let expectedStack = tupleElems ++ baseStack
          pure $
            counterexample ("Tuple: " ++ show tupleElems) $
              checkInstr initStack expectedStack StackUnpack

    it "inverts StackPack" $
      property $
        \baseStack (Positive n) -> do
          topStack <- vector n
          let initStack = topStack ++ baseStack
          pure $
            counterexample ("Tuple: " ++ show topStack) $
              -- Just the initial stack
              checkInstrs initStack initStack [StackPack n, StackUnpack]

  describe "StackCond" $ do
    -- TODO Also test with function references

    it "checks numeric condition" $
      property $
        \baseStack cond a b -> do
          condValue <- if cond then getPositive <$> arbitrary else pure 0
          let result = fromIntegral (if cond then a else b)
          let instr = StackCond (StackLiteral a) (StackLiteral b)
          let initStack = StackScalar condValue : baseStack
          let expectedStack = StackScalar result : baseStack
          pure $
            counterexample ("Condition value: " ++ show condValue) $
              checkInstr initStack expectedStack instr

    it "checks tuple condition" $
      property $
        \baseStack (NonEmpty tupleElems) a b ->
          -- Tuples should always be true
          let tuple = StackTuple (fromList tupleElems)
              result = fromIntegral a
              instr = StackCond (StackLiteral a) (StackLiteral b)
              initStack = tuple : baseStack
              expectedStack = StackScalar result : baseStack
           in counterexample ("Condition value: " ++ show tuple) $
                checkInstr initStack expectedStack instr

  describe "StackDup" $
    it "copies value" $
      property $
        \(NonEmpty initStack) -> do
          index <- choose (1, length initStack)
          let copied = initStack !! (index - 1)
          let expectedStack = copied : initStack
          pure $
            counterexample ("Index: " ++ show index) $
              checkInstr initStack expectedStack (StackDup index)

  describe "StackDel" $
    it "deletes value" $
      property $
        \(NonEmpty initStack) -> do
          index <- choose (1, length initStack)
          let (before, _ : after) = splitAt (index - 1) initStack
          let expectedStack = before ++ after
          pure $
            counterexample ("Index: " ++ show index) $
              checkInstr initStack expectedStack (StackDel index)

{- TODO Figure out a good way to handle non-printable-ascii strings
pinky's tests use a custom String newtype, but that isn't really ideal

describe "StackInput" $ do
  it "inputs character" $
    property $
      \initStack input ->
        let (tape, (remainingInput, output)) =
              runStackInstr emptyContext initStack input StackInput
            (char : inputTail) = input
            Just value = fromChar char
            expectedStack = StackScalar value : initStack
            expectedTape = makeStackTape expectedStack
         in ((expectedTape :: BfState Word8), inputTail)
              === (tape, remainingInput)

describe "StackOutput" $ do
  it "outputs character" $
    property $
      \baseStack char ->
        let Just value = fromChar char
            initStack = StackScalar value : baseStack
            (tape, (_, output)) =
              runStackInstr emptyContext initStack "" StackInput
            expectedStack = StackScalar value : initStack
            expectedTape = makeStackTape expectedStack
         in ((expectedTape :: BfState Word8), inputTail)
              === (tape, remainingInput)
-}
