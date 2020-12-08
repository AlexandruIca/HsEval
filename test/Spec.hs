module Main where

import RPNTest
import Test.Tasty
import TokenizerTest

allTests :: TestTree
allTests = testGroup " All tests! " [testGroup "TokenizerTest" tokenizerTests, testGroup "RPNTest" rpnTests]

main :: IO ()
main = do defaultMain allTests
