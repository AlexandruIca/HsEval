module Main where

import Test.Tasty
import TokenizerTest

allTests :: TestTree
allTests = testGroup " All tests! " $ [testGroup "TokenizerTest" tokenizerTests]

main :: IO ()
main = do defaultMain allTests
