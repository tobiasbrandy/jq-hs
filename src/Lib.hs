module Lib (
  parseFilter
, repl
) where

import Prelude hiding (filter)

import Data.Json (Json)
import Data.Filter (Filter)

import Parse.Defs (ParserState, ParserResult (..), parserHasNext, parserRun)

import Parse.Json.Tokens (JsonToken)
import Parse.Json.Parser (jsonParser)

import Parse.Filter.Tokens (FilterToken)
import Parse.Filter.Parser (filterParser)

import Data.Text (Text)


parseFilter :: ParserState FilterToken -> Either Text Filter
parseFilter state = case parserRun state filterParser of
  Error msg       -> Left msg
  Ok (_, filter)  -> Right filter

-- Read -> Eval -> Process -> Loop
-- In order to be flexible, structure is similar to foldl'.
-- This allows to reduce json values however we see fit.
-- First function argument must handle the error case.
-- Return type is a monad, as we need a way to handle both success and error.
-- Monad is executed strictly (ASAP). This allows processing data as a stream.
-- Examples, print jsons/error with IO () or collect them in a list with Either Text [Json]
repl :: Monad m => (Text -> m t) -> (t -> Json -> m t) -> t -> ParserState JsonToken -> m t
repl errF jsonF = run
  where
    run ret state =
      if parserHasNext state
      then
        case parserRun state jsonParser of
          Error msg           -> errF msg
          Ok (newState, json) -> do
            ret' <- jsonF ret json
            ret' `seq` run ret' newState
      else
        return ret

