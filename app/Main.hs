module Main where

import Options (getOptions)

import Parse.Defs (LexState, lexStateInit, ParseResult (..), lexRun, lexHasNext)
import Parse.Json.Parser (parseJson)
import Json (Json)
import Parse.Json.Tokens (Token)
import Json.Encode (encode, encodePretty, Config (..), Indent (..), NumberFormat (..))

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Control.Monad (when)
import Text.Pretty.Simple (pPrint)
import Data.Text.Encoding (encodeUtf8)


main :: IO ()
main = do
  options <- getOptions
  pPrint options
  stdIn <- BS.getContents
  repl (lexStateInit stdIn)

repl :: LexState Token -> IO ()
repl state =
  when (lexHasNext state) $
    case lexRun state parseJson of
      Error msg           -> (BSS.putStr . encodeUtf8) msg
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
