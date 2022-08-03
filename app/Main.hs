module Main where

import Parsing.Defs (LexState, lexStateInit, ParseResult (..), lexRun, lexHasNext)

import Parsing.Json.Parser (parseJson)
import Json (Json)
import Parsing.Json.Tokens (Token)

import Text.Pretty.Simple (pPrint)
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
      Error msg           -> pPrint msg
      Ok (newState, json) -> do
        processJson json
        repl newState

processJson :: Json -> IO ()
processJson = pPrint
