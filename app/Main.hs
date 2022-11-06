module Main where

import Prelude hiding (filter, seq)

import Options (getOptions, Options (..), Indent (..), FilterInput (..), Color (..))

import Data.Parser.Parse (parseOne, parseAll, parserStateInit)

import Data.Filter (Filter (..))
import Data.Filter.Builtins (builtins)
import Data.Filter.Run (filterRunExp, FilterRet (..), FilterResult)
import Data.Filter.Parsing.Parser (filterParser)

import Data.Json (Json (..))
import Data.Json.Encode (jsonEncode, compactFormat, Format (..), Indent (..))
import Data.Json.Parsing.Parser (jsonParser)

import qualified Data.Sequence as Seq

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import System.IO (Handle, stdin, stdout, stderr)
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
      writeFilterOutput opts $ filterRunExp builtins filter j
      run js

writeFilterOutput :: Options -> FilterResult Json -> IO ()
writeFilterOutput opts = go 
  where
    go []             = return ()
    go (Ok json:xs)   = do
      writeJson opts stdout json
      go xs
    go (Err msg:_)    = writeError msg
    go (Halt c msg:_) = do
      when (isJust msg) $
        BS.hPut stderr $ jsonEncode compactFormat { fmtRawStr = True } $ fromJust msg
      exitWith $ if c == 0 then ExitSuccess else ExitFailure c
    go (Stderr msg:xs) = do
      BS.hPut stderr $ jsonEncode compactFormat { fmtRawStr = True } msg
      go xs
    go (Debug msg:xs) = do
      let debugJson = Array $ Seq.fromList [String "DEBUG:", msg]
      writeJson opts { indent = Options.Spaces 0, joinOut = False } stderr debugJson
      go xs

writeJson :: Options -> Handle -> Json -> IO ()
writeJson Options {..} handle = BS.hPut handle . jsonEncode Format
    { fmtIndent           = indentOpt2Fmt indent
    , fmtCompare          = if sortKeys then compare else mempty
    , fmtRawStr           = rawOut || joinOut
    , fmtColorize         = colorize colorOut
    , fmtTrailingNewline  = not joinOut
  }
  where
    indentOpt2Fmt  Options.Tab       = Data.Json.Encode.Tab
    indentOpt2Fmt (Options.Spaces n) = Data.Json.Encode.Spaces n

    colorize (CDefault b) = b
    colorize CEnabled     = True
    colorize CDisabled    = False

writeFilterParserError :: Text -> IO ()
writeFilterParserError msg = endWithStatus 1 $ "compile error: " <> msg

writeJsonParserError :: Text -> IO ()
writeJsonParserError msg = endWithStatus 2 $ "parser error: " <> msg

endWithStatus :: Int -> Text -> IO ()
endWithStatus code msg = do
  writeError msg
  exitWith $ ExitFailure code

writeError :: Text -> IO ()
writeError msg = do
  BSS.hPut stderr $ encodeUtf8 ("jqhs: error: " <> msg)
  BSS.hPut stderr "\n"
