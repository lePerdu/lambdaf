{-# LANGUAGE OverloadedStrings #-}

module LambdaF
  ( CompilerError (..),
    compileLambdaF,
    defaultPrimitives,
    module LambdaF.Language,
    module LambdaF.Parser,
    module LambdaF.Compiler,
    module LambdaF.Printer,
  )
where

import Data.Either.Combinators
import qualified Data.HashMap.Strict as M
import LambdaF.Language
import LambdaF.Compiler
import LambdaF.Parser
import LambdaF.Printer
import LambdaF.StackIr
import Pinky.Brainfuck.Language

defaultPrimitives :: [PrimOp]
defaultPrimitives =
  [ PrimOp "add" 2 [StackPrim $ StackIrPrim "add" stackAdd],
    PrimOp "sub" 2 [StackPrim $ StackIrPrim "sub" stackSub],
    PrimOp "pair" 2 [StackPack 2],
    PrimOp "fst" 1 [StackUnpack, StackDel 2],
    PrimOp "snd" 1 [StackUnpack, StackDel 1],
    PrimOp "getchar" 1 [StackInput, StackPack 2],
    PrimOp "putchar" 2 [StackOutput]
  ]

data CompilerError = LambdaFCompErr LambdaFError | StackIrCompErr StackIrError
  deriving (Show)

-- | Compiles a full LambdaF program
--
-- The program should be of type (IO -> IO) using defaultPrimitives defined
-- here.
compileLambdaF :: LambdaF -> Either CompilerError Bf
compileLambdaF lam = do
  -- Apply to a "fake" IO value
  let lamIo = Appl lam (Constant 0)
  ir <- mapLeft LambdaFCompErr $ lambdaFToStackIr lamIo
  bf <- mapLeft StackIrCompErr $ stackIrToBf ir
  return bf
