module Main where

import Prelude hiding (seq)

import Options (Options (..), getOptions, colorize)
import qualified Options as Opt (Indent (..))

import Lib (repl)

import Parse.Defs (parserStateInit)
import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, Format (..), NumberFormat (..))
import qualified Data.Json.Encode as Enc (Indent (..))

import Data.Sequence (Seq (..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
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
writeJson Options {..} = BS.putStr . jsonEncode fmt
  where
    fmt = Format {
      fmtIndent           = indentOpt2Fmt indent,
      fmtCompare          = if sortKeys then compare else mempty,
      fmtNumFormat        = Generic,
      fmtRawStr           = rawOut || joinOut,
      fmtColorize         = colorize colorOut,
      fmtTrailingNewline  = not joinOut
    }
    indentOpt2Fmt  Opt.Tab       = Enc.Tab
    indentOpt2Fmt (Opt.Spaces n) = Enc.Spaces n

