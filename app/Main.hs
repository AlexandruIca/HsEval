module Main where

import Tokenizer
import Parser

expression :: String
expression = "(1 + 256 - 3 / 4 * 5 + 5^(2^1))"

--expression :: String
--expression = "2^3 + 10 + 3 * (7 - 15 / 3)"

main :: IO ()
main = do { putStrLn $ "Lexing for: " ++ expression ++ "\n" ++ show (tokenize expression)
          ; putStrLn $ "Postfix: " ++ show ((rpn . tokenize) expression)
          ; putStrLn $ "Eval for " ++ expression ++ " = " ++ show (evalPostfix expression)
          }
