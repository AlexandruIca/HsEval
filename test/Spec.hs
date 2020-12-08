module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TokenizerTest

main :: IO ()
main = do defaultMain (testGroup "Tokenizer tests" tokenizerTests)
