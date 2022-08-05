module Main where

import Options (Options (..), getOptions, colorize, colorSetDefault)
import qualified Options as Opt (Indent (..))

import Parse.Defs (ParserState, parserStateInit, ParserResult (..), parserRun, parserHasNext)
import Parse.Json.Parser (jsonParser)
import Json (Json (..))
import Parse.Json.Tokens (Token)
import Json.Encode (jsonEncode, jsonStrQuote, jsonEncodePretty, Config (..), NumberFormat (..))
import qualified Json.Encode as Enc (Indent (..))

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BSS
import Control.Monad (unless)
import Text.Pretty.Simple (pPrint)
import Data.Text.Encoding (encodeUtf8)
import System.IO (hIsTerminalDevice, stdin, stdout, hFlush)
import Data.Text (Text)

main :: IO ()
main = do
  opts <- getOptions
  tty <- hIsTerminalDevice stdout
  let opts' = setColorDefault tty opts
  pPrint opts
  input <- BS.hGetContents stdin
  -- repl errorHandle (jsonProcessor opts') (jsonProcessorBase opts') (parserStateInit input)
  repl errorHandle (jsonProcessor opts') (jsonProcessorBase opts') (parserStateInit input)

-- Read -> Eval -> Process -> Loop
-- In order to be flexible, structure is similar to foldl'
-- This allows to reduce json values however we see fit. 
-- For ex, print them or collect them in a list.
repl :: (Text -> t) -> (t -> Json -> t) -> t -> ParserState Token -> t
repl errF jsonF = run
  where
    run ret state =
      if parserHasNext state
      then
        case parserRun state jsonParser of
          Error msg           -> errF msg
          Ok (newState, json) -> let ret' = jsonF ret json in
            ret' `Prelude.seq` run ret' newState
      else
        ret

errorHandle :: Text -> IO ()
errorHandle = BSS.putStr . encodeUtf8

jsonProcessor :: Options -> IO () -> Json -> IO ()
jsonProcessor opts ret json = do
  ret
  writeJson opts json

jsonProcessorBase :: Options -> IO ()
jsonProcessorBase = const $ return ()

writeJson :: Options -> Json -> IO ()
writeJson opts@Options {..} json = do
  BS.putStr $
    ( if compactOut
        then jsonEncode
      else if rawOut || joinOut
        then rawOutput
      else
        buildJsonPrettyOutput opts
    ) json
  unless joinOut $
    BS.putStr $ BS.singleton 10 -- print newline

rawOutput :: Json -> ByteString
rawOutput (String s) = jsonStrQuote s
rawOutput json = jsonEncode json

buildJsonPrettyOutput :: Options -> Json -> ByteString
buildJsonPrettyOutput Options {..} = jsonEncodePretty Config {
  confIndent            = indentOpt2Conf indent,
  confCompare           = if sortKeys then compare else mempty,
  confNumFormat         = Generic,
  confTrailingNewline   = False,
  confColorizeTerminal  = colorize colorOut
}

indentOpt2Conf :: Opt.Indent -> Enc.Indent
indentOpt2Conf  Opt.Tab       = Enc.Tab
indentOpt2Conf (Opt.Spaces n) = Enc.Spaces n

setColorDefault :: Bool -> Options -> Options
setColorDefault b opts@Options { colorOut } = opts { colorOut = colorSetDefault b colorOut }


