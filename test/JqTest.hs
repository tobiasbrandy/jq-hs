{-# LANGUAGE TemplateHaskell #-}

module JqTest (jqTest) where

import Test.HUnit (Test (..), assertEqual)

import Data.Filter (Filter)
import Data.Filter.Run (filterRunExp)
import Data.Json (Json)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Parse.Defs (parserStateInit, parserRun, ParserResult (..))
import Parse.Filter.Parser (filterParser)
import Data.FileEmbed (embedFile)
import Data.List (groupBy)
import Data.Filter.Builtins (builtins)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function ((&))
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Encoding (decodeUtf8With)
import Lib (repl)
import Data.Functor.Identity (runIdentity, Identity (..))

jqTest :: Test
jqTest = TestLabel "JQ Encoded Tests" $ TestList
  [ baseTests
  , manualTests
  , utf8TruncateTests
  ]

---------------------------------- JQ Tests ------------------------------------ 

baseTests :: Test
baseTests = TestLabel "Base" $ parseTests $(embedFile "test/jq/base.jq.test")

manualTests :: Test
manualTests = TestLabel "Manual" $ parseTests $(embedFile "test/jq/manual.jq.test")

utf8TruncateTests :: Test
utf8TruncateTests = TestLabel "UTF-8 Truncate" $ parseTests $(embedFile "test/jq/utf8-truncate.jq.test")

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

failOnErr :: Either Text Json -> Json
failOnErr (Left msg) = error $ T.unpack msg
failOnErr (Right json) = json

parseJson :: ByteString -> [Json]
parseJson input = reverse $ runIdentity $ repl (error . ("Error during json parsing: " <>) . T.unpack) (\js -> Identity . (:js)) [] (parserStateInit input)

parseFilter :: ByteString -> Filter
parseFilter input = case parserRun (parserStateInit input) filterParser of
  Ok (_, ret) -> ret
  Error msg   -> error $ "Error during filter parsing: " <> T.unpack msg

showBS :: ByteString -> String
showBS = T.unpack  . decodeUtf8With lenientDecode . BS.toStrict
