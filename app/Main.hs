module Main where

import qualified Lexer as L
import qualified Parser as P

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = pPrint $ L.runAlex "let the_answer : int =\n  let a = 20 in\n  let b = 1 in\n  let c = 2 fdsfin\n  a * c + b * c\n\nlet main (unit : ()) : () =\n  print (\"The answer is: \" + the_answer)" P.parseMiniML
