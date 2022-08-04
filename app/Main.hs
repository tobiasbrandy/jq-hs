module Main where

import Parsing.Defs (LexState, lexStateInit, ParseResult (..), lexRun, lexHasNext)

import Parsing.Json.Parser (parseJson)
import Json (Json)
import Parsing.Json.Tokens (Token)
import Json.Encode (encode, encodePretty, Config (..), Indent (..), NumberFormat (..))

import qualified Data.ByteString.Lazy as BS
import Control.Monad (when)

main :: IO ()
main = do
  stdIn <- BS.getContents
  repl (lexStateInit stdIn)

repl :: LexState Token -> IO ()
repl state =
  when (lexHasNext state) $
    case lexRun state parseJson of
      Error msg           -> print msg
      Ok (newState, json) -> do
        processJson json
        repl newState

processJson :: Json -> IO ()
processJson = prettyPrint

prettyPrint :: Json -> IO ()
prettyPrint = BS.putStr . encodePretty Config {
  confIndent            = Spaces 2,
  confCompare           = mempty,
  confNumFormat         = Generic,
  confTrailingNewline   = True,
  confColorizeTerminal  = True
}

compactPrint :: Json -> IO ()
compactPrint = BS.putStr . encode

