{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit (runTestTT, Test (..), assertEqual, showCounts)

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

newLine :: Word8
newLine = 10

hashtag :: Word8
hashtag = 35

main :: IO ()
main = do
  counts <- runTestTT $ TestList jqTests
  putStrLn $ showCounts counts

jqTests :: [Test]
jqTests
  = $(embedFile "test/jq.test")
  & zip [0..] . BSS.split newLine
  & filter (\(_, s) -> BSS.null s || BSS.head s /= hashtag)
  & groupBy (\(_, l) (_, r) -> let nl = BSS.null l; nr = BSS.null r in (nl && nr) || not (nl || nr))
  & filter (not . BSS.null . snd . head)
  & map (\((i, x):xs) -> testDefToTest (i, BS.fromStrict x : map (BS.fromStrict . snd) xs))

testDefToTest :: (Int, [ByteString]) -> Test
testDefToTest (line, program : input : output) = TestCase $
  assertEqual ("Test line " <> show line)
    (map errToString $ filterRunExp builtins (parseFilter program) (parseJson input))
    (map parseJson output)
testDefToTest xs = error $ "Malformed test: " <> show xs

-- tests :: [Test]
-- tests = [
--   TestLabel "testObjectFilter" $ TestCase $
--     assertEqual "null -> {'hola': 1}" 
--     [ json "{\"1\": \"1\"}"
--     , json "{\"1\": \"2\"}"
--     , json "{\"1\": \"3\"}"
--     , json "{\"2\": \"1\"}"
--     , json "{\"2\": \"2\"}"
--     , json "{\"2\": \"3\"}"
--     , json "{\"3\": \"1\"}"
--     , json "{\"3\": \"2\"}"
--     , json "{\"3\": \"3\"}"
--     ] $
--     rights $ filterRunExp Map.empty (filter "{(.[]): .[]}") (json "[\"1\",\"2\", \"3\"]")
--   ]

errToString :: Either Text Json -> Json
errToString (Left msg) = String msg
errToString (Right json) = json

parseJson :: ByteString -> Json
parseJson input = fromJust $ parserOkResult $ parserRun (parserStateInit input) jsonParser

parseFilter :: ByteString -> Filter
parseFilter input = parserOkResult $ parserRun (parserStateInit input) filterParser

parserOkResult :: ParserResult token result -> result
parserOkResult (Ok (_, ret)) = ret
parserOkResult (Error msg) = error $ "Error during parsing: " <> T.unpack msg

