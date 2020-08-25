{-# LANGUAGE OverloadedStrings #-}

module LambdaF.StackIr.Printer
  ( printStackIr,
  )
where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import LambdaF.StackIr.Language
import Pinky.Brainfuck

instance Pretty StackValue where
  pretty (StackIdent t) = pretty t
  pretty (StackLiteral x) = pretty x

prettyIfNot :: (Pretty c, Eq c) => c -> c -> Doc ann
prettyIfNot x c = if c == x then emptyDoc else pretty c

prettyInstr :: Pretty c => T.Text -> [c] -> Doc ann
prettyInstr name args =
  pretty name <+> hsep (punctuate comma (map pretty args))

prettyInstrDef :: (Pretty c, Eq c) => T.Text -> c -> c -> Doc ann
prettyInstrDef name def arg = pretty name <+> prettyIfNot def arg

instance Pretty StackIrInstr where
  pretty (StackPush v) = prettyInstr "push" [v]
  pretty (StackDup n) = prettyInstrDef "dup" 1 n
  pretty (StackDel n) = prettyInstrDef "del" 1 n
  pretty StackInput = pretty ("in" :: T.Text)
  pretty StackOutput = pretty ("out" :: T.Text)
  pretty (StackCond t f) = prettyInstr "cond" [t, f]
  pretty (StackPack n) = prettyInstr "pack" [n]
  pretty StackUnpack = pretty ("unpack" :: T.Text)
  pretty (StackPrim (StackIrPrim name _)) = pretty name

instance Pretty StackIr where
  pretty (StackIr funcs) = vsep $ punctuate line printedFuncs
    where
      prettyLabeled leader (StackIrFunc label code) =
        nest 4 $ vcat $ pretty leader <+> pretty label : map pretty code

      prettyLabelAll leader funcs = map (prettyLabeled leader) funcs

      printedFuncs = prettyLabelAll ':' funcs

printStackIr :: StackIr -> T.Text
printStackIr = renderStrict . layoutPretty defaultLayoutOptions . pretty
