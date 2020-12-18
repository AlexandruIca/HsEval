module Main where

import Parser
import RecursiveDescent
import Tokenizer

expression :: String
expression = "(1 + 256 - 3 / 4 * 5 + 5^(2^1))"

expression2 :: String
expression2 = "sin(asin(0)) + sqrt(5 * exp(2))"

main :: IO ()
main = do
  putStrLn $ "Lexing for: " ++ expression ++ "\n" ++ show (tokenize expression)
  putStrLn $ "Postfix: " ++ show ((rpn . tokenize) expression)
  putStrLn $ "Eval for " ++ expression ++ " = " ++ show (evalPostfix expression)
  putStrLn $ "Calc for " ++ expression2 ++ " = " ++ show (calc expression2)