module Main where

-- import Data.Either.Combinators

import Data.Either.Combinators
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import LambdaF
import LambdaF.StackIr
import Options.Applicative
import System.Exit
import Pinky.Brainfuck.Language
import System.FilePath
import Text.Megaparsec (errorBundlePretty)

data Options = Options
  { _outputFileName :: Maybe String,
    _fileName :: String
  }

optParser :: Parser Options
optParser =
  Options
    <$> optional
      ( option
          str
          (short 'o' <> metavar "OUTPUT_FILE" <> help "output file")
      )
    <*> argument str (metavar "FILE" <> help "file to compile")

optInfo :: ParserInfo Options
optInfo = info (optParser <**> helper) (fullDesc <> progDesc "")

compile :: LambdaF -> Either String Bf
compile = mapLeft show . compileLambdaF

main :: IO ()
main = do
  options <- execParser optInfo
  let fileName = _fileName options
  let outFileName =
        fromMaybe
          (replaceExtensions fileName "bf")
          (_outputFileName options)

  contents <- readFile fileName
  case parseLambdaF defaultPrimitives fileName (T.pack contents) of
    Left e -> putStrLn (errorBundlePretty e)
    Right result -> case compile result of
      Left e -> die e
      Right bf -> writeFile outFileName (printBf bf)
  pure ()
