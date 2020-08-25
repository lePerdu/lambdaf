{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaF.Parser
  ( lambdaProgram,
    parseLambdaF,
  )
where

import Control.Monad.Reader
import Data.Either.Combinators
import Data.Char
import qualified Data.HashMap.Strict as M
import Data.List
import qualified Data.Text as T
import Data.Void
import LambdaF.Language
import Lens.Micro.Platform
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

-- | Mapping from primitive name to the primitive itself
type PrimitivesMap = M.HashMap T.Text PrimOp

data Binding = TermBinding | RecTermBinding

type Bindings = [(T.Text, Binding)]

data Precedence = TopLevel | ApplPrec

data Context = Context
  { _parserPrims :: PrimitivesMap,
    _parserBindings :: Bindings
  }

makeLenses ''Context

type Parser = ParsecT Void T.Text (Reader Context)

lineComment :: Parser ()
lineComment = Lex.skipLineComment "--"

skipSpace :: Parser ()
skipSpace = Lex.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme skipSpace

symbol :: T.Text -> Parser T.Text
symbol = Lex.symbol skipSpace

withBinding :: T.Text -> Binding -> Parser a -> Parser a
withBinding name bindingType =
  local (parserBindings %~ ((name, bindingType) :))

reservedIdent :: T.Text -> Bool
reservedIdent = (`elem` ["λ", "letrec", "cond"])

ident :: Parser T.Text
ident = lexeme $ do
  start <- satisfy (\c -> isLetter c || c == '_')
  rest <- takeWhileP (Just "identifier body") (\c -> isAlphaNum c || c == '_')
  let full = T.cons start rest
  if reservedIdent full
    then fail $ "Reserved identifier: `" ++ T.unpack full ++ "`"
    else pure full

term :: Parser LambdaF
term = do
  name <- ident
  -- Lookup as binding, then as a primitive
  ctx <- ask
  case findBinding name (_parserBindings ctx) of
    Just binding -> pure $ binding
    Nothing -> case M.lookup name (_parserPrims ctx) of
      Just prim -> pure $ Prim prim
      Nothing -> fail $ "Unknown identifier: `" ++ T.unpack name ++ "`"
  where
    findBinding name = go (0, 0)
      where
        go _ [] = Nothing
        go (regs, recs) ((binding, bindType) : rest) =
          if binding == name
            then Just $ makeBinding bindType regs recs
            else case bindType of
              TermBinding -> go (regs + 1, recs) rest
              RecTermBinding -> go (regs, recs + 1) rest

    makeBinding TermBinding termIndex _ = Term termIndex
    makeBinding RecTermBinding _ recIndex = RecTerm recIndex

number :: Parser Int
number = lexeme Lex.decimal

constant :: Parser LambdaF
constant = Constant <$> number

lambdaChar :: Parser ()
lambdaChar = void (symbol "\\" <|> symbol "λ")

lambda :: Parser LambdaF
lambda = do
  lambdaChar
  name <- ident
  symbol "."
  Lambda <$> withBinding name TermBinding (lambdaExpr TopLevel)

letrec :: Parser LambdaF
letrec = do
  symbol "letrec"
  name <- ident
  symbol "="
  Letrec <$> withBinding name RecTermBinding (lambdaExpr TopLevel)

cond :: Parser LambdaF
cond = do
  symbol "cond"
  Cond <$> expr <*> expr <*> expr
  where
    expr = lambdaExpr ApplPrec

parens :: Parser LambdaF
parens =
  between (symbol "(") (symbol ")") $ lambdaExpr TopLevel

basic :: Parser LambdaF
basic = parens <|> constant <|> term

leftAssocTerm :: Parser LambdaF
leftAssocTerm = cond <|> basic

applicationChain :: Parser LambdaF
applicationChain = do
  first <- leftAssocTerm
  rest <- many (lambdaExpr ApplPrec)
  pure $ foldl' (\a b -> Appl a b) first rest

lambdaExpr :: Precedence -> Parser LambdaF
lambdaExpr TopLevel = lambda <|> letrec <|> applicationChain
lambdaExpr ApplPrec = basic

lambdaProgram :: Parser LambdaF
lambdaProgram = lambdaExpr TopLevel <* eof

parseLambdaF ::
  [PrimOp] ->
  String ->
  T.Text ->
  Either (ParseErrorBundle T.Text Void) LambdaF
parseLambdaF prims fileName contents =
  runReader (runParserT lambdaProgram fileName contents) initContext
  where
    primMap = M.fromList $ map (\p -> (_primName p, p)) prims
    initContext = Context primMap []
