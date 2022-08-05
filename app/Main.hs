module Main where

import Options (Options (..), getOptions, colorize)
import qualified Options as Opt (Indent (..))

import Lib (repl)

import Parse.Defs (parserStateInit)
import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, jsonStrQuote, jsonEncodePretty, Config (..), NumberFormat (..))
import qualified Data.Json.Encode as Enc (Indent (..))

import Data.Sequence (Seq (..))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BSS
import Control.Monad (unless)
import Data.Text.Encoding (encodeUtf8)
import System.IO (stdin)
import Data.Text (Text)

main :: IO ()
main = do
  opts@Options { slurp } <- getOptions

  input <- BS.hGetContents stdin
  let state = parserStateInit input

  if slurp
  then
    case repl Left (\js -> Right . (:|>) js) Empty state of
      Left err    -> writeError err
      Right jsons -> writeJson opts $ Array jsons
  else
    repl writeError (const $ writeJson opts) () state 

writeError :: Text -> IO ()
writeError = BSS.putStr . encodeUtf8

writeJson :: Options -> Json -> IO ()
writeJson opts@Options { compactOut, rawOut, joinOut } json = do
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
  -- json `Prelude.seq` putStrLn "1"
  -- hFlush stdout

rawOutput :: Json -> ByteString
rawOutput (String s) = jsonStrQuote s
rawOutput json = jsonEncode json

buildJsonPrettyOutput :: Options -> Json -> ByteString
buildJsonPrettyOutput Options { indent, sortKeys, colorOut } = jsonEncodePretty Config {
  confIndent            = indentOpt2Conf indent,
  confCompare           = if sortKeys then compare else mempty,
  confNumFormat         = Generic,
  confTrailingNewline   = False,
  confColorizeTerminal  = colorize colorOut
}

indentOpt2Conf :: Opt.Indent -> Enc.Indent
indentOpt2Conf  Opt.Tab       = Enc.Tab
indentOpt2Conf (Opt.Spaces n) = Enc.Spaces n
