module Main where

import Prelude hiding (filter, seq)

import Options (Options (..), Indent (..), FilterInput (..), getOptions, colorize)

import Lib (parseFilter, repl)

import Parse.Defs (parserStateInit)

import Data.Filter (Filter (..), runFilter)

import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, Format (..), Indent (..), NumberFormat (..))

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import System.IO (stdin)
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
  opts@Options {..} <- getOptions

  filterOrErr <- getFilter opts
  filterOrErr `ifError` endWithStatus 1 $ \filter -> do
    let processJson = mapM_ (writeJson opts) . runFilter filter

    if nullInput
    then
      processJson Null
    else do
      input <- BS.hGetContents stdin
      let state = parserStateInit input

      if slurp
      then
        repl Left (\js -> Right . (:|>) js) Seq.empty state `ifError` endWithStatus 2 $
        processJson . Array
      else
        repl (endWithStatus 2) (const processJson) () state

getFilter :: Options -> IO (Either Text Filter)
getFilter Options {..} = case filterInput of
  Literal input -> return $ Right input
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
  putStrLn ""
  exitWith $ ExitFailure code

ifError :: Either a b -> (a -> c) -> (b -> c) -> c
ifError e errF continueF = either errF continueF e
