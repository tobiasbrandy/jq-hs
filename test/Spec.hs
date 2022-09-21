{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit (Test (..), runTestTTAndExit, assertEqual)

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

main :: IO ()
main = runTestTTAndExit $ TestList [baseTests, manualTests, utf8TruncateTests]

baseTests :: Test
baseTests = TestLabel "JQ Base Tests" $ parseTests $(embedFile "test/base.jq.test")

manualTests :: Test
manualTests = TestLabel "JQ Manual Tests" $ parseTests $(embedFile "test/manual.jq.test")

utf8TruncateTests :: Test
utf8TruncateTests = TestLabel "UTF-8 Truncate Tests" $ parseTests $(embedFile "test/utf8-truncate.jq.test")

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
