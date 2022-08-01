-- Alex interface definition requirements --
module Parsing.Internal.Lexing.AlexIntegration (
  AlexInput
, ignorePendingBytes
, alexInputPrevChar
, genAlexGetByte
) where

import Parsing.Defs (Lex, lexError, LexAction, LexPos (..), LexInput, lexGetInput, lexSetInput, StartCode, lexGetStartCode, lexSetStartCode)
import Parsing.Internal.Lexing.Utils (tok, textTok, strTok, numTok)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c) -- Should be a nop

import Data.Word (Word8)

type AlexInput = LexInput

-- no pending bytes when lexing bytestrings
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes = id

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, prev, _, _) = prev

-- Generate alexGetByte function given tab size
genAlexGetByte :: Int -> AlexInput -> Maybe (Word8, AlexInput)
genAlexGetByte tab_size (pos, _, bs, n) =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (b, bs') ->
      let
        c     = BS.w2c b
        pos'  = updatePos tab_size pos c
        n'    = n+1
      in pos' `seq` bs' `seq` n' `seq` Just (b, (pos', c, bs', n'))

-- Get updated lexer position
updatePos :: Int -> LexPos -> Char -> LexPos
updatePos tab_size (LexPos a l c) '\t' = LexPos (a+1)  l  (c + tab_size - ((c-1) `mod` tab_size))
updatePos _ (LexPos a l _) '\n' = LexPos (a+1) (l+1)   1
updatePos _ (LexPos a l c) _    = LexPos (a+1)  l     (c+1)
