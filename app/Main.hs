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
  opts <- getOptions
  filterOrErr <- getFilter opts
  case filterOrErr of
    Left msg      -> writeFilterParserError msg
    Right filter  -> do
      jsons <- getJsons opts
      processJsons opts filter jsons

getFilter :: Options -> IO (Either Text Filter)
getFilter Options {..} = case filterInput of
  Arg input -> return $ parseOne filterParser $ parserStateInit $ BS.fromStrict $ encodeUtf8 input
  File path -> do
    input <- BS.readFile path
    return $ parseOne filterParser $ parserStateInit input

getJsons :: Options -> IO [Either Text Json]
getJsons Options {..} =
  if nullInput
  then
    return [Right Null]
  else do
    jsons <- case inputFiles of
      []  -> parseAll jsonParser id . parserStateInit <$> BS.hGetContents stdin
      _   -> concatMap (parseAll jsonParser id . parserStateInit) <$> mapM BS.readFile inputFiles    

    if slurp
    then return $
      case sequence jsons of
        Left msg      -> [Left msg]
        Right jsons'  -> [Right $ Array $ Seq.fromList jsons']        
    else
      return jsons

processJsons :: Options -> Filter -> [Either Text Json] -> IO ()
processJsons opts filter = run
  where
    run []            = return ()
    run (Left msg:_)  = writeJsonParserError msg
    run (Right j:js)  = do
      writeFilter opts $ filterRunExp builtins filter j
      run js

writeFilter :: Options -> FilterResult Json -> IO ()
writeFilter opts = go 
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

writeFilterParserError :: Text -> IO ()
writeFilterParserError msg = endWithStatus 1 $ "compile error: " <> msg

writeJsonParserError :: Text -> IO ()
writeJsonParserError msg = endWithStatus 2 $ "parser error: " <> msg

endWithStatus :: Int -> Text -> IO ()
endWithStatus code msg = do
  writeError msg
  exitWith $ ExitFailure code
