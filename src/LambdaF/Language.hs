{-# LANGUAGE DeriveGeneric #-}

module LambdaF.Language
  ( LambdaF (..),
    LambdaFCps (..),
    LambdaComb (..),
    PrimOp (..),
  )
where

import Data.Hashable
import qualified Data.Text as T
import LambdaF.StackIr.Language
import GHC.Generics (Generic)

-- | Primitive operator
--
-- TODO Should primitives instead be done by parameterizing LambdaF (and
-- related types)? This helps to ensure all of the primitives "work together"
-- as they are defined as a single set. Likewise for StackIr primitives?
data PrimOp = PrimOp
  { _primName :: !T.Text,
    _primArgCount :: !Int,
    _primCode :: [StackIrInstr]
  }
  deriving (Show)

instance Hashable PrimOp where
  -- Just base hash off the name
  hashWithSalt = hashUsing _primName

-- | Core lambda calculus language
data LambdaF
  = Term !Int
  | RecTerm !Int
  | Constant !Int
  | Cond LambdaF LambdaF LambdaF
  | Appl LambdaF LambdaF
  | Lambda LambdaF
  | Letrec LambdaF
  | Prim PrimOp
  deriving (Show)

-- | CPS lambda calculus representation
--
-- TODO Should/can this and regular LambdaF be parameterizations of a generic
-- LambdaExpr type to abstract away the common parts like lambdas, application,
-- constants, etc.
data LambdaFCps
  = CTerm !Int
  | CRecTerm !Int
  | CConstant !Int
  | CCond LambdaFCps LambdaFCps
  | CAppl LambdaFCps LambdaFCps
  | CLambda LambdaFCps
  | CLetrec LambdaFCps
  | CPrim PrimOp LambdaFCps
  deriving (Show)

-- | A set of stack-based combinators to get rid of lambdas.
--
-- In all of the instructions, 1 refers to the top of the stack (like in
-- StackIr).
-- TODO Should StackIr be changed to 0-indexed to match DeBruijn indices? Vice-
-- versa?
data LambdaComb
  = LId
  | LConst !Int
  | LRecTerm !Int
  | LLetrec LambdaComb
  | LCond LambdaComb LambdaComb
  | LPush LambdaComb LambdaComb
  | LDup !Int LambdaComb
  | LDel !Int LambdaComb
  | LMkcl !Int !Int LambdaComb
  | LPrim PrimOp LambdaComb
  deriving (Show, Generic)

instance Hashable LambdaComb
