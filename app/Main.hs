module Main where

import Parsing.Defs (lexRun)

import Parsing.Filter.Tokens (Token (..))
import Parsing.Filter.Lexer (lexer)

import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)

main :: IO ()
main = do
  contents <- BS.getContents
  case scanMany contents of
    Left msg    -> pPrint msg
    Right tkns  -> pPrint tkns


scanMany :: ByteString -> Either Text [Token]
scanMany input = lexRun input go
  where
    go = do
      output <- lexer
      if output == EOF
        then pure [output]
        else (output :) <$> go
