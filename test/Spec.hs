module Main where

import Test.Tasty
import TokenizerTest
import RPNTest

allTests :: TestTree
allTests = testGroup " All tests! " [testGroup "TokenizerTest" tokenizerTests, testGroup "RPNTest" rpnTests]

main :: IO ()
main = do defaultMain allTests
