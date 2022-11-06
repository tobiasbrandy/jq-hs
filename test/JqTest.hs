{-# LANGUAGE TemplateHaskell #-}
module JqTest (jqTest) where

import Test.HUnit (Test (..), assertEqual)

import Data.Filter (Filter)
import Data.Filter.Run (filterRunExp, FilterRet)
import qualified Data.Filter.Run as Ret (FilterRet (Ok))
import Data.Filter.Parsing.Parser (filterParser)
import Data.Json (Json)
import Data.Json.Parsing.Parser (jsonParser)

import Data.Parser.Parse (parseOne, parseAll, parserStateInit)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Data.FileEmbed (embedFile)
import Data.List (groupBy)
import Data.Filter.Builtins (builtins)
import qualified Data.Text as T
import Data.Function ((&))
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Encoding (decodeUtf8With)

jqTest :: Test
jqTest = TestLabel "JQ Encoded Tests" $ TestList
  [ baseTest
  , manualTest
  , utf8TruncateTest
  -- , datesTests
  , regexTest
  ]

---------------------------------- JQ Tests ------------------------------------ 

baseTest :: Test
baseTest = TestLabel "Base" $ parseTests $(embedFile "test/jq/base.jq.test")

manualTest :: Test
manualTest = TestLabel "Manual" $ parseTests $(embedFile "test/jq/manual.jq.test")

utf8TruncateTest :: Test
utf8TruncateTest = TestLabel "UTF-8 Truncate" $ parseTests $(embedFile "test/jq/utf8-truncate.jq.test")

-- datesTest :: Test
-- datesTest = TestLabel "Date Builtins" $ parseTests $(embedFile "test/jq/dates.jq.test")

regexTest :: Test
regexTest = TestLabel "Oniguruma Regex Builtins" $ parseTests $(embedFile "test/jq/regex.jq.test")

------------------------------- JQ Test Parsing --------------------------------- 

parseTests :: BSS.ByteString -> Test
parseTests file = TestList
  $ file
  & zip [0..] . BSS.split newLine
  & filter (\(_, s) -> BSS.null s || BSS.head s /= hashtag)
  & groupBy (\(_, l) (_, r) -> let nl = BSS.null l; nr = BSS.null r in (nl && nr) || not (nl || nr))
  & filter (not . BSS.null . snd . head)
  & map (\((i, x):xs) -> testDefToTest (i, BS.fromStrict x : map (BS.fromStrict . snd) xs))
  where
    newLine = 10
    hashtag = 35

testDefToTest :: (Int, [ByteString]) -> Test
testDefToTest (line, program : input : output) = TestCase $
  assertEqual ("Line " <> show line <> "; echo '" <> showBS input <> "' | jqhs '" <> showBS program <> "'")
    (concatMap parseJson output)
    (map failOnErr $ concatMap (filterRunExp builtins (parseFilter program)) (parseJson input))
testDefToTest xs = error $ "Malformed test: " <> show xs

failOnErr :: FilterRet Json -> Json
failOnErr (Ret.Ok json) = json
failOnErr other = error $ show other

parseJson :: ByteString -> [Json]
parseJson input = map (either (error . ("Error during json parsing: " <>) . T.unpack) id) $ parseAll jsonParser id $ parserStateInit input
-- parseJson input = reverse $ runIdentity $ repl (error . ("Error during json parsing: " <>) . T.unpack) (\js -> Identity . (:js)) [] (parserStateInit input)

parseFilter :: ByteString -> Filter
parseFilter input = case parseOne filterParser $ parserStateInit input of
  Left msg  -> error $ "Error during filter parsing: " <> T.unpack msg
  Right ret -> ret

showBS :: ByteString -> String
showBS = T.unpack  . decodeUtf8With lenientDecode . BS.toStrict
