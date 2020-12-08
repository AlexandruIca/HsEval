module Main where

import Parser
import Tokenizer

expression :: String
expression = "(1 + 256 - 3 / 4 * 5 + 5^(2^1))"

main :: IO ()
main = do
  putStrLn $ "Lexing for: " ++ expression ++ "\n" ++ show (tokenize expression)
  putStrLn $ "Postfix: " ++ show ((rpn . tokenize) expression)
  putStrLn $ "Eval for " ++ expression ++ " = " ++ show (evalPostfix expression)
