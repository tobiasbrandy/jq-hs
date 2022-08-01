-- Usefull functions when defining lexing rules in lexer
module Parsing.Internal.Lexing.Utils (
  LexAction
, tok
, textTok
, strTok
, numTok
) where

import Parsing.Defs (LexAction)

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Char (chr)

tok :: token -> LexAction token
tok t _ _ = return t

textTok :: (Text -> token) -> LexAction token
textTok f (_, _, str, _) len = return $ f $ decodeUtf8 $ BS.toStrict $ BS.take len str

-- Same as textTok but drops first and last chars. Intended for quoted strings.
strTok :: (Text -> token) -> LexAction token
strTok f (_, _, str, _) len = return $ f $ decodeUtf8 $ BS.toStrict $ BS.take (len-2) $ BS.drop 1 str

-- TODO(tobi): Fijarse si se puede hacer el parseo sin pasar por string
numTok :: (Either Integer Double -> token) -> LexAction token
numTok f (_, _, str, _) len = return $ f $ readNum $ map (chr . fromEnum) . BS.unpack $ BS.take len str

readNum :: String -> Either Integer Double
readNum s = let d = (read s :: Double) in
  if isNaN d || isInfinite d
  then Right d
  else let n = truncate d in
    if d == fromInteger n then Left n else Right d
