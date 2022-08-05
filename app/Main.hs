module Main where

import Options (Options (..), getOptions)

import Parse.Defs (ParserState, parserStateInit, ParserResult (..), parserRun, parserHasNext)
import Parse.Json.Parser (jsonParser)
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
  opt@Options {..} <- getOptions
  pPrint opt
  stdIn <- BS.getContents
  repl (parserStateInit stdIn)

repl :: ParserState Token -> IO ()
repl state =
  when (parserHasNext state) $
    case parserRun state jsonParser of
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
