module Main where

import Lexer.Defs (lexRun)
import Lexer.Engine (lexer)
import Lexer.Tokens (Token (..))

import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  contents <- BS.getContents
  pPrint $ scanMany contents

scanMany :: ByteString -> Either String [Token]
scanMany input = lexRun input go
  where
    go = do
      output <- lexer
      if output == EOF
        then pure [output]
        else (output :) <$> go
