{-# LANGUAGE TemplateHaskell #-}

module LambdaF.StackIr.Language
  ( StackValue (..),
    StackIrInstr (..),
    StackIrPrim (..),
    stackPrimName,
    stackPrimCode,
    StackIrFunc (..),
    StackIr (..),
    irFunctions,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import LambdaF.StackIr.StackBf
import Pinky.Brainfuck.Language
import Lens.Micro.Platform

-- | Constant which can be pushed to the stack
--
-- This can be either a literal value or a reference to a defined function.
data StackValue = StackIdent !T.Text | StackLiteral !Int deriving (Show, Eq)

-- | Primitive operator description
--
-- Primitive operators should
-- * Only operate on scalar values (i.e. not tuples)
-- * Assume their arguments are on the top of the stack and all scalars
-- * Maintina normal invariants, such as leaving pseudo cells unmarked and
--   leaving tags alone
data StackIrPrim = StackIrPrim
  {_stackPrimName :: !T.Text, _stackPrimCode :: StackBfM ()}

makeLenses ''StackIrPrim

instance Show StackIrPrim where
  show = T.unpack . _stackPrimName

-- | Single instruction
--
-- Indices start from 1 being the top of the stack (and hence 0 is never a
-- valid index).
data StackIrInstr
  = -- | Push a constant to the stack
    StackPush !StackValue
  | -- | Duplicate the n-th item to the top of the stack
    StackDup !Int
  | -- | Delete the n-th item from the stack
    StackDel !Int
  | -- | Inupt a character and push it to the stack
    StackInput
  | -- | Pop the top of the stack and output it
    StackOutput
  | -- | Conditionally push a value to the stack
    --
    -- If the top of the stack is non-0, the first value will be pushed;
    -- otherwise the second one will be pushed.
    StackCond !StackValue !StackValue
  | -- | Pack the top n items into a tuple
    StackPack !Int
  | -- | Unpack a tuple on the top of the stack
    --
    -- TODO Define behavior when the top isn't a tuple.
    StackUnpack
  | -- | Primitive operator
    StackPrim StackIrPrim
  deriving (Show)

-- | Stack IR function
data StackIrFunc = StackIrFunc !T.Text [StackIrInstr] deriving (Show)

-- | Full StackIr program
newtype StackIr = StackIr {_irFunctions :: [StackIrFunc]} deriving (Show)

makeLenses ''StackIr
