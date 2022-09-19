{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit (Test (..), runTestTTAndExit, assertEqual)

import Data.Filter (Filter (..))
import Data.Filter.Run (filterRunExp)
import Data.Json (Json (..))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Parse.Defs (parserStateInit, parserRun, ParserResult (..))
import Parse.Json.Parser (jsonParser)
import Parse.Filter.Parser (filterParser)
import Data.Maybe (fromJust)
import Data.FileEmbed (embedFile)
import Data.Word (Word8)
import Data.List (groupBy)
import Data.Filter.Builtins (builtins)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function ((&))
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Encoding (decodeUtf8With)

newLine :: Word8
newLine = 10

hashtag :: Word8
hashtag = 35

main :: IO ()
main = runTestTTAndExit baseTests

baseTests :: Test
baseTests = TestLabel "JQ Base Tests" $ TestList
  $ $(embedFile "test/jq.test")
  & zip [0..] . BSS.split newLine
  & filter (\(_, s) -> BSS.null s || BSS.head s /= hashtag)
  & groupBy (\(_, l) (_, r) -> let nl = BSS.null l; nr = BSS.null r in (nl && nr) || not (nl || nr))
  & filter (not . BSS.null . snd . head)
  & map (\((i, x):xs) -> testDefToTest (i, BS.fromStrict x : map (BS.fromStrict . snd) xs))

testDefToTest :: (Int, [ByteString]) -> Test
testDefToTest (line, program : input : output) = TestCase $
  assertEqual ("Line " <> show line <> "; echo '" <> showBS input <> "' | jqhs '" <> showBS program <> "'")
    (map parseJson output)
    (map failOnErr $ filterRunExp builtins (parseFilter program) (parseJson input))
testDefToTest xs = error $ "Malformed test: " <> show xs

failOnErr :: Either Text Json -> Json
failOnErr (Left msg) = error $ T.unpack msg
failOnErr (Right json) = json

parseJson :: ByteString -> Json
parseJson input = fromJust $ parserOkResult $ parserRun (parserStateInit input) jsonParser

parseFilter :: ByteString -> Filter
parseFilter input = parserOkResult $ parserRun (parserStateInit input) filterParser

parserOkResult :: ParserResult token result -> result
parserOkResult (Ok (_, ret)) = ret
parserOkResult (Error msg) = error $ "Error during parsing: " <> T.unpack msg

showBS :: ByteString -> String
showBS = T.unpack  . decodeUtf8With lenientDecode . BS.toStrict
