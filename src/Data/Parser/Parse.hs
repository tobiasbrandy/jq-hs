module Data.Parser.Parse 
( Parser
, parseOne
, parseAll

, ParserState
, parserStateInit
) where

import Data.Parser.Build.Parser
  ( Parser

  , ParserState
  , parserStateInit

  , ParserResult (..)
  , parserRun
  , parserHasNext
  )

import Data.Text (Text)

-- Parse exactly one result
parseOne :: Parser token result -> ParserState token -> Either Text result
parseOne parser state = case parserRun state parser of
  Error msg   -> Left msg
  Ok (_, ret) -> Right ret

-- Parse all results. Second argument is a function to map or discard results
parseAll :: Parser token result -> (result -> Maybe t) -> ParserState token -> [Either Text t]
parseAll parser mapper = runParser
  where
    runParser state =
      if parserHasNext state
      then
        case parserRun state parser of
          Error msg           -> [Left msg]
          Ok (newState, mret) -> maybe [] (\ret -> Right ret : runParser newState) $ mapper mret
      else
        []
