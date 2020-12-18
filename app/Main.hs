module Main where

import Control.Monad
import Parser
import RecursiveDescent
import System.IO
import Tokenizer

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

prettyPrint :: Show a => Either a String -> String
prettyPrint (Right r) = r
prettyPrint (Left l) = show l

replPostfix :: IO ()
replPostfix = do
  str <- prompt "eval> "
  print . prettyPrint $ evalPostfix str

replCalc :: IO ()
replCalc = do
  str <- prompt "eval> "
  print . prettyPrint $ calc str

main :: IO ()
main = do
  putStrLn "Press Ctrl+C to exit"
  forever replCalc