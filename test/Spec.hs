module Main where

import EvalPostfixTest
import RPNTest
import Test.Tasty
import TokenizerTest

allTests :: TestTree
allTests =
  testGroup
    " All tests! "
    [ testGroup "TokenizerTest" tokenizerTests,
      testGroup "RPNTest" rpnTests,
      testGroup "EvalPostfixTest" evalPostfixTests
    ]

main :: IO ()
main = do defaultMain allTests
