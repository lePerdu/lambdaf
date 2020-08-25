{-# LANGUAGE OverloadedStrings #-}

module LambdaF.StackIr.Parser
  ( stackIr,
    parseStackIr,
  )
where

import Data.Char
import Data.Functor
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Void
import LambdaF.StackIr.Language
import Pinky.Brainfuck.Parser (parseBf)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void T.Text

isHorizSpace :: Char -> Bool
isHorizSpace = (`elem` (" \t\v" :: String))

horizSpace :: Parser ()
horizSpace = void $ takeWhileP Nothing isHorizSpace

horizSpace1 :: Parser ()
horizSpace1 = void $ takeWhile1P Nothing isHorizSpace

isEolSlash :: Char -> Bool
isEolSlash c = c == '\n' || c == '\\'

eolSlash :: Parser ()
eolSlash = void eol <|> void (char '\\')

lineComment :: Parser ()
lineComment = Lex.skipLineComment ";"

skipLine :: Parser ()
skipLine = lexemeNL (horizSpace >> (eolSlash <|> lineComment))

-- | Lexeme now allowing newline
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme horizSpace

-- | Lexeme allowing for newline
lexemeNL :: Parser x -> Parser x
lexemeNL = Lex.lexeme (Lex.space space1 lineComment empty)

symbol :: T.Text -> Parser T.Text
symbol = Lex.symbol horizSpace

symbol' :: T.Text -> Parser T.Text
symbol' = Lex.symbol' horizSpace

ident :: Parser T.Text
ident = lexeme $ do
  start <- satisfy (\c -> isLetter c || c == '_')
  rest <- takeWhileP (Just "identifier body") (\c -> isAlphaNum c || c == '_')
  pure (T.cons start rest)

comma :: Parser ()
comma = void $ lexeme (char ',')

literal :: Parser Int
literal =
  lexeme $
    Lex.signed
      horizSpace
      ( Lex.decimal
          <|> (char '0' *> char' 'x' *> Lex.hexadecimal)
          <|> (char '0' *> char' 'b' *> Lex.binary)
          <|> (char '0' *> Lex.octal)
      )

literalOr1 :: Parser Int
literalOr1 = literal <|> pure 1

value :: Parser StackValue
value = (StackIdent <$> ident) <|> (StackLiteral <$> literal)

instr :: Parser StackIrInstr
instr =
  ( push
      <|> dup
      <|> del
      <|> input
      <|> output
      <|> cond
      <|> pack
      <|> unpack
  )
    <* skipLine
  where
    push = (symbol' "push" $> StackPush) <*> value
    dup = (symbol' "dup" $> StackDup) <*> literalOr1
    del = (symbol' "del" $> StackDel) <*> literalOr1
    input = symbol' "in" $> StackInput
    output = symbol' "out" $> StackOutput
    cond = (symbol' "cond" $> StackCond) <*> value <* comma <*> value
    pack = (symbol' "pack" $> StackPack) <*> literal
    unpack = symbol' "unpack" $> StackUnpack

function :: Parser StackIrFunc
function = do
  symbol ":"
  name <- ident
  skipLine
  code <- many (lexemeNL instr)
  return $ StackIrFunc name code

stackIr :: Parser StackIr
stackIr = StackIr <$> many function

parseStackIr ::
  String ->
  T.Text ->
  Either (ParseErrorBundle T.Text Void) StackIr
parseStackIr fileName contents = runParser stackIr fileName contents
