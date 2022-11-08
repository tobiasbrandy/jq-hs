module Main where

import Prelude hiding (filter, seq)

import Options (getOptions, Options (..), Indent (..), FilterInput (..), Color (..))

import Data.Parser.Parse (parseOne, parseAll, parserStateInit)

import Data.Filter (Filter (..))
import Data.Filter.Builtins (builtins)
import Data.Filter.Run (filterRunExp, FilterRet (..), FilterResult)
import Data.Filter.Parsing.Parser (filterParser)

import Data.Json (Json (..), jsonBool)
import Data.Json.Encode (jsonEncode, compactFormat, Format (..), Indent (..), defaultColors, parseColors)
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
import System.Environment (lookupEnv, getEnvironment)

main :: IO ()
main = do
  opts <- getOptions
  filterOrErr <- getFilter opts
  case filterOrErr of
    Left msg      -> do
      writeFilterParserError msg
      exitWith $ ExitFailure 3
    Right filter  -> do
      jsons <- getJsons opts
      exitCode <- processJsons opts filter jsons
      exitWith exitCode

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

processJsons :: Options -> Filter -> [Either Text Json] -> IO ExitCode
processJsons opts@Options { exitStatus } filter = run (if exitStatus then ExitFailure 4 else ExitSuccess)
  where
    run exitCode [] = return exitCode
    run _ (Left msg:_) = do
      writeJsonParserError msg
      return $ ExitFailure 4
    run exitCode (Right j:js) = do
      env <- getEnvironment
      let result = filterRunExp env builtins filter j
      writeFilterOutput opts result
      let newExitCode
            | null result = exitCode
            | otherwise   = case last result of
              Ok json   -> if exitStatus && not (jsonBool json) then ExitFailure 1 else ExitSuccess
              Err _     -> ExitFailure 5
              Halt c _  -> if c == 0 then ExitSuccess else ExitFailure c
              Stderr _  -> error "processJsons: Stderr cannot be last return value of filter"
              Debug _   -> error "processJsons: Debug cannot be last return value of filter"
      run newExitCode js

writeFilterOutput :: Options -> FilterResult Json -> IO ()
writeFilterOutput opts = go
  where
    go []             = return ()
    go (Ok json:xs)   = do
      writeJson opts stdout json
      go xs
    go (Err msg:_)    = writeError $ "error: " <> msg
    go (Halt _ msg:_) = do
      when (isJust msg) $
        BS.hPut stderr $ jsonEncode compactFormat { fmtRawStr = True } $ fromJust msg
    go (Stderr msg:xs) = do
      BS.hPut stderr $ jsonEncode compactFormat { fmtRawStr = True } msg
      go xs
    go (Debug msg:xs) = do
      let debugJson = Array $ Seq.fromList [String "DEBUG:", msg]
      writeJson opts { indent = Options.Spaces 0, joinOut = False } stderr debugJson
      go xs

writeJson :: Options -> Handle -> Json -> IO ()
writeJson Options {..} handle json = do
  colors <- getColors
  BS.hPut handle $ jsonEncode Format
    { fmtIndent           = indentOpt2Fmt indent
    , fmtCompare          = if sortKeys then compare else mempty
    , fmtRawStr           = rawOut || joinOut
    , fmtColors           = colors
    , fmtTrailingNewline  = not joinOut
  } json
  where
    indentOpt2Fmt  Options.Tab       = Data.Json.Encode.Tab
    indentOpt2Fmt (Options.Spaces n) = Data.Json.Encode.Spaces n

    getColors =
      if colorize colorOut
      then do
        colorsStr <- lookupEnv "JQHS_COLORS"
        return $ Just $ maybe defaultColors parseColors colorsStr
      else return Nothing

    colorize (CDefault b) = b
    colorize CEnabled     = True
    colorize CDisabled    = False

writeFilterParserError :: Text -> IO ()
writeFilterParserError msg = writeError $ "compile error: " <> msg

writeJsonParserError :: Text -> IO ()
writeJsonParserError msg = writeError $ "parse error: " <> msg

writeError :: Text -> IO ()
writeError msg = do
  BSS.hPut stderr $ encodeUtf8 ("jqhs: " <> msg)
  BSS.hPut stderr "\n"
