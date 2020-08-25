module LambdaF.StackIr
  ( stackAdd,
    stackSub,
    module LambdaF.StackIr.Language,
    module LambdaF.StackIr.Compiler,
    module LambdaF.StackIr.Parser,
    module LambdaF.StackIr.Printer,
    module LambdaF.StackIr.StackBf,
  )
where

import LambdaF.StackIr.Compiler
import LambdaF.StackIr.Language
import LambdaF.StackIr.Parser
import LambdaF.StackIr.Printer
import LambdaF.StackIr.StackBf

-- | Adds the top 2 elements of the stack
stackAdd :: StackBfM ()
stackAdd = do
  stackLoop ValueCell $ do
    incrementValue (-1)
    movePseudoCell (-2)
    incrementValue 1
    movePseudoCell 2

  -- Pop the top element
  movePseudoCell (-2)

-- | Subtracts the top 2 elements of the stack
stackSub :: StackBfM ()
stackSub = do
  -- Because the first argument is on the top of the stack, the result has to be
  -- built in the top cell, then moved into the next one

  -- Do subtraction
  movePseudoCell (-2)
  stackLoop ValueCell $ do
    incrementValue (-1)
    movePseudoCell 2
    incrementValue (-1)
    movePseudoCell (-2)

  -- Copy back
  movePseudoCell 2
  stackLoop ValueCell $ do
    incrementValue (-1)
    movePseudoCell (-2)
    incrementValue 1
    movePseudoCell 2

  -- Pop top element
  movePseudoCell (-2)
