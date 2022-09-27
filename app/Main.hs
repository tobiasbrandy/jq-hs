module Main where

import Prelude hiding (filter, seq)

import Options (Options (..), Indent (..), FilterInput (..), getOptions, colorize)

import Data.Parser.Parse (parseOne, parseAll, parserStateInit)

import Data.Filter (Filter (..))
import Data.Filter.Builtins (builtins)
import Data.Filter.Run (filterRunExp, FilterRet (..), FilterResult)
import Data.Filter.Parsing.Parser (filterParser)

import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, Format (..), Indent (..))
import Data.Json.Parsing.Parser (jsonParser)

import qualified Data.Sequence as Seq

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import System.IO (stdin)
import System.Exit (ExitCode (..), exitWith)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
  opts@Options {..} <- getOptions

  filterOrErr <- getFilter opts
  filterOrErr `ifError` endWithStatus 1 $ \filter -> do
    let run = processJson opts filter

    if nullInput
    then
      run [Right Null]
    else do
      input <- BS.hGetContents stdin
      let jsons = parseAll jsonParser id $ parserStateInit input

      if slurp
      then
        case sequence jsons of
          Left msg      -> writeParserError msg
          Right jsons'  -> run [Right $ Array $ Seq.fromList jsons']        
      else
        run jsons

processJson :: Options -> Filter -> [Either Text Json] -> IO ()
processJson opts filter = run
  where
    run [] = return ()
    run (Left msg:_) = writeParserError msg
    run (Right j:js) = do
      writeFilterOut opts $ filterRunExp builtins filter j
      run js

getFilter :: Options -> IO (Either Text Filter)
getFilter Options {..} = case filterInput of
  Arg input     -> return $ parseOne filterParser $ parserStateInit $ BS.fromStrict $ encodeUtf8 input
  File path     -> do
    input <- BS.readFile path
    return $ parseOne filterParser $ parserStateInit input

writeFilterOut :: Options -> FilterResult Json -> IO ()
writeFilterOut opts = go 
  where
    go []             = return ()
    go (Ok json:xs)   = do writeJson opts json; go xs
    go (Err msg:_)    = writeError msg
    go (Halt c msg:_) = do
      when (isJust msg) $
        BSS.putStr $ encodeUtf8 $ fromJust msg
      exitWith $ ExitFailure c

writeJson :: Options -> Json -> IO ()
writeJson Options {..} = BS.putStr . jsonEncode fmt
  where
    fmt = Format
      { fmtIndent           = indentOpt2Fmt indent
      , fmtCompare          = if sortKeys then compare else mempty
      , fmtRawStr           = rawOut || joinOut
      , fmtColorize         = colorize colorOut
      , fmtTrailingNewline  = not joinOut
    }
    indentOpt2Fmt  Options.Tab       = Data.Json.Encode.Tab
    indentOpt2Fmt (Options.Spaces n) = Data.Json.Encode.Spaces n

writeError :: Text -> IO ()
writeError msg = do
  BSS.putStr $ encodeUtf8 ("jqhs: error: " <> msg)
  putStrLn ""

writeParserError :: Text -> IO ()
writeParserError msg = endWithStatus 2 $ "parser error: " <> msg

endWithStatus :: Int -> Text -> IO ()
endWithStatus code msg = do
  writeError msg
  exitWith $ ExitFailure code

ifError :: Either a b -> (a -> c) -> (b -> c) -> c
ifError e errF continueF = either errF continueF e
