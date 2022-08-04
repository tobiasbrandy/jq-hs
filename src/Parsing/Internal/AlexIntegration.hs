-- Alex interface definition requirements --
module Parsing.Internal.AlexIntegration (
  AlexInput
, alexGetByte
) where

import Parsing.Defs (LexInput, posForward)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c) -- Should be a nop

import Data.Word (Word8)

type AlexInput = LexInput

-- Generate alexGetByte function given tab size
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, n, bs) =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (b, bs') ->
      let
        pos'  = posForward pos (BS.w2c b)
        n'    = n+1
      in pos' `seq` n' `seq` bs' `seq` Just (b, (pos', n', bs'))

