module Main where

import Lexer.Defs (lexRun)
import Lexer.Engine (lexer)
import Lexer.Tokens (Token (..))

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
