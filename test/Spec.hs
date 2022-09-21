import Test.HUnit (Test (..), runTestTTAndExit)

import JqTest (jqTest)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ jqTest
  ]
