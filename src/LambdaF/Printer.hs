module LambdaF.Printer
  ( printLambdaF,
    printLambdaFCps,
  )
where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import LambdaF.Language

prec :: Int -> Int -> Doc ann -> Doc ann
prec outterP innerP
  | outterP > innerP = parens
  | otherwise = id

instance Pretty LambdaF where
  pretty = prettyPrec 0
    where
      prettyPrec _ (Term x) = pretty ("t" ++ show x)
      prettyPrec _ (RecTerm x) = pretty ("r" ++ show x)
      prettyPrec _ (Constant c) = pretty c
      prettyPrec p (Cond c t f) =
        prec p 1 $
          pretty "cond" <+> hsep (map (prettyPrec 2) [c, t, f])
      prettyPrec p (Appl f v) =
        prec p 1 $ prettyPrec 1 f <+> prettyPrec 2 v
      prettyPrec p (Lambda f) =
        prec p 0 $ pretty "λ." <+> prettyPrec 0 f
      prettyPrec p (Letrec f) = prec p 0 $ pretty "letrec =" <+> prettyPrec 0 f
      prettyPrec p (Prim prim) = pretty (_primName prim)

instance Pretty LambdaFCps where
  pretty = prettyPrec 0
    where
      prettyPrec _ (CTerm x) = pretty ("t" ++ show x)
      prettyPrec _ (CRecTerm x) = pretty ("r" ++ show x)
      prettyPrec _ (CConstant c) = pretty c
      prettyPrec p (CCond t f) =
        prec p 1 $ pretty "cond" <+> hsep (map (prettyPrec 2) [t, f])
      prettyPrec p (CAppl f v) =
        prec p 1 $ prettyPrec 1 f <+> prettyPrec 2 v
      prettyPrec p (CLambda f) = prec p 0 $ pretty "λ." <+> prettyPrec 0 f
      prettyPrec p (CLetrec f) =
        prec p 0 $ pretty "letrec =" <+> prettyPrec 0 f
      prettyPrec p (CPrim prim cont) =
        prec p 1 $ pretty (_primName prim) <+> prettyPrec 2 cont

printLambdaF :: LambdaF -> T.Text
printLambdaF = renderStrict . layoutPretty defaultLayoutOptions . pretty

printLambdaFCps :: LambdaFCps -> T.Text
printLambdaFCps = renderStrict . layoutPretty defaultLayoutOptions . pretty
