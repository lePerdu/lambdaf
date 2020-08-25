{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaF.StackIr.FunctionSpec where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Char
import qualified Data.HashMap.Strict as M
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word
import qualified Data.Text as T
import LambdaF.StackIr.Compiler
import LambdaF.StackIr.Language
import LambdaF.StackIr.StackBf
import LambdaF.StackIr.TestUtils
import Pinky.Brainfuck
import Test.QuickCheck
import Test.Hspec

emptyContext :: IndexedIr
emptyContext = IndexedIr mempty

funcToStackBf :: Int -> [StackIrInstr] -> Either StackIrError (StackBfM ())
funcToStackBf index instrs =
  runExcept $ runReaderT (irFunctionToBf index instrs) emptyContext

runStackFunc ::
  forall c.
  BfCell c =>
  [StackElem c] ->
  Int ->
  [StackIrInstr] ->
  [c]
runStackFunc stack index instrs =
  let Right stackBf = funcToStackBf index instrs
      stackBf' = stackBf >> moveStackCell ValueCell
      bf = intoBf $ stackBfIntoBf ValueCell stackBf'

      initialTapeAction = makeInitTape $ makeStack stack
      bfAction = initialTapeAction >> interpretBasicTape bf

      ((_, finalTape), _) = runBufferMachine (runBfTape bfAction) ""
   in truncateStack finalTape

checkFunc ::
  [StackElem Word8] -> [StackElem Word8] -> Int -> [StackIrInstr] -> Property
checkFunc initStack expectedStack index instrs =
  let tape = runStackFunc initStack index instrs
      expectedTape = makeStackTape expectedStack
   in expectedTape === tape

spec :: Spec
spec = do
  context "with scalar function index" $ do
    it "decrements non-matching function index" $
      property $
        \baseStack (Positive (index :: Int)) (Positive other) ->
          other /= 1
            ==> let initStack = StackScalar other : baseStack
                    expectedStack = StackScalar (other - 1) : baseStack
                 in checkFunc initStack expectedStack (fromIntegral index) []

    it "decrements continuation function index" $
      property $
        \baseStack (Positive index) (Positive next) ->
          next /= index
            ==> let initStack = StackScalar 1 : StackScalar next : baseStack
                    expectedStack =
                      StackScalar (next - index) : baseStack
                 in checkFunc initStack expectedStack (fromIntegral index) []

    it "recurses on self" $
      property $
        \baseStack (Positive index) (Positive next) ->
          next /= index
            ==> let initStack =
                      StackScalar 1 :
                      StackScalar index :
                      StackScalar next :
                      baseStack
                    expectedStack = StackScalar (next - index) : baseStack
                 in checkFunc initStack expectedStack (fromIntegral index) []

  context "with tuple function index" $ do
    it "unpacks single tuple before function call" $
      property $
        \baseStack tupleArgs (Positive index) (Positive next) ->
          next /= 1
            ==> let closure =
                      StackTuple
                        (StackScalar 1 :| StackScalar next : tupleArgs)
                    initStack = closure : baseStack
                    expectedStack =
                      StackScalar (next - index) : (tupleArgs ++ baseStack)
                 in checkFunc initStack expectedStack (fromIntegral index) []

    it "unpacks nested tuple before function call" $ do
      property $
        \baseStack
         (tupleArgs :: [StackElem Word8])
         (Positive index)
         (Positive next) ->
            next /= 1
              ==> let inner = StackTuple (StackScalar 1 :| [StackScalar next])
                      -- Wrap in pairs with the function in the inner-most,
                      -- left-most pair
                      closure =
                        foldl' (\a b -> StackTuple (a :| [b])) inner tupleArgs
                      initStack = closure : baseStack
                      -- All of the tuples should be unpacked
                      expectedStack =
                        StackScalar (next - index) : (tupleArgs ++ baseStack)
                   in checkFunc initStack expectedStack (fromIntegral index) []
