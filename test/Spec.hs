import Prelude hiding (filter)

import Test.HUnit (runTestTT, Test (..), assertEqual, showCounts)

import Data.Filter (runFilter, Filter (..))
import Data.Json (Json (..))

import Data.ByteString.Lazy (ByteString)
import Parse.Defs (parserStateInit, parserRun, ParserResult (..))
import Parse.Json.Parser (jsonParser)
import Parse.Filter.Parser (filterParser)


main :: IO ()
main = do
  counts <- runTestTT $ TestList tests
  putStrLn $ showCounts counts

tests :: [Test]
tests = [
  TestLabel "testObjectFilter" $ TestCase $
    assertEqual "null -> {'hola': 1}" 
    [ json "{\"1\": \"1\"}"
    , json "{\"1\": \"2\"}"
    , json "{\"1\": \"3\"}"
    , json "{\"2\": \"1\"}"
    , json "{\"2\": \"2\"}"
    , json "{\"2\": \"3\"}"
    , json "{\"3\": \"1\"}"
    , json "{\"3\": \"2\"}"
    , json "{\"3\": \"3\"}"
    ] $
    runFilter (filter "{(.[]): .[]}") (json "[\"1\",\"2\", \"3\"]")
  ]

json :: ByteString -> Json
json input = parserOkResult $ parserRun (parserStateInit input) jsonParser

filter :: ByteString -> Filter
filter input = parserOkResult $ parserRun (parserStateInit input) filterParser

parserOkResult :: ParserResult token result -> result
parserOkResult (Ok (_, ret)) = ret
parserOkResult _ = error "Error during parsing"

