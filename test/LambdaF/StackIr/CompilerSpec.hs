{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaF.StackIr.CompilerSpec where

import Data.Char
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.Word
import LambdaF.StackIr.Compiler
import LambdaF.StackIr.Language
import LambdaF.StackIr.StackBf
import Pinky.Brainfuck
import LambdaF.StackIr.TestUtils
import Test.Hspec

-- TODO Make generic
runStackIr :: String -> StackIr -> (String, String)
runStackIr input code =
  let Right bf = stackIrToBf code
   in execBufferMachine (interpretBasic bf :: BufferMachine Word8 ()) input

printIrFunc :: T.Text -> Char -> StackIrFunc
printIrFunc name c =
  StackIrFunc
    name
    [StackPush (StackLiteral (ord c)), StackOutput]

spec :: Spec
spec = do
  describe "main function" $ do
    it "empty main exits" $
      runStackIr "" (StackIr [StackIrFunc "main" []])
        `shouldBe` ("", "")

    it "main is called as only function" $
      runStackIr "" (StackIr [printIrFunc "main" '0']) `shouldBe` ("", "0")

    -- it "main is called when first function" $
    --   runStackIr
    --     ""
    --     (StackIr [printIrFunc "main" '0', printIrFunc "other" '1'])
    --     `shouldBe` ("", "0")

    it "main is called as second function" $
      runStackIr
        ""
        (StackIr [printIrFunc "other" '1', printIrFunc "main" '0'])
        `shouldBe` ("", "0")
