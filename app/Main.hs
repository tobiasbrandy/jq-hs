module Main where

import Prelude hiding (filter, seq)

import Options (Options (..), Indent (..), FilterInput (..), getOptions, colorize)

import Lib (parseFilter, repl)

import Parse.Defs (parserStateInit)

import Data.Filter (Filter (..))

import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, Format (..), Indent (..), NumberFormat (..))

import Data.Sequence (Seq (..))

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import System.IO (stdin)
import System.Exit (ExitCode (..), exitWith)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  opts@Options {..} <- getOptions

  filterOrErr <- getFilter opts
  filterOrErr `ifError` endWithStatus 1 $ \filter -> do
    pPrint filter

    input <- BS.hGetContents stdin
    let state = parserStateInit input

    if slurp
    then
      repl Left (\js -> Right . (:|>) js) Empty state `ifError` endWithStatus 2 $
      writeJson opts . Array
    else
      repl (endWithStatus 2) (const $ writeJson opts) () state

getFilter :: Options -> IO (Either Text Filter)
getFilter Options {..} = case filterInput of
  Options.Null  -> return $ Right Data.Filter.Null
  Arg input     -> return $ parseFilter $ parserStateInit $ BS.fromStrict input
  File path     -> do
    input <- BS.readFile path
    return $ parseFilter $ parserStateInit input

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
    indentOpt2Fmt  Options.Tab       = Data.Json.Encode.Tab
    indentOpt2Fmt (Options.Spaces n) = Data.Json.Encode.Spaces n

endWithStatus :: Int -> Text -> IO ()
endWithStatus code msg = do
  BSS.putStr $ encodeUtf8 msg
  exitWith $ ExitFailure code

ifError :: Either a b -> (a -> c) -> (b -> c) -> c
ifError e errF continueF = either errF continueF e
