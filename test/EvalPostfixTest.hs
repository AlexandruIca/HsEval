module EvalPostfixTest
  ( evalPostfixTests,
  )
where

import Parser
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase)
import Tokenizer

tests :: [(String, Double)]
tests =
  [ ("", 0.0),
    ("   ", 0.0),
    ("\n\t\r ", 0.0),
    ("100", 100.0),
    ("1+2", 3.0),
    ("3 * 4 / 2 ^ 7", 0.0),
    ("(3 * 4 / 2) ^ 7", 6.0 ** 7.0),
    ("2 ^ 2 ^ 3", 256.0),
    ("2 * 4 * 3", 24.0),
    ("2 ^ 4 * 3", 48.0),
    ("2 ^ 4 - 3", 13.0)
  ]

evalPostfixTests :: [TestTree]
evalPostfixTests = map toTestTree tests
  where
    filterInput :: String -> String
    filterInput s = filter (`notElem` " \n\r\t") s

    makeResult :: InterpreterResult -> Double -> (Int, Int)
    makeResult (Left result) expectedResult = (round result, round expectedResult)
    makeResult (Right _) _ = (1, 2)

    isEq :: (String, Double) -> Bool
    isEq (s, result) = uncurry (==) (makeResult (evalPostfix s) result)

    assertMsg :: String -> Double -> String
    assertMsg s expected = "`evalPostfix` failed for '" ++ s ++ "'\n\t\t expected: " ++ show expected ++ "\n\t\t output: " ++ show (evalPostfix s)

    toTestTree :: (String, Double) -> TestTree
    toTestTree input = testCase ("EvalPostfix[" ++ filterInput (fst input) ++ "]") (assertBool (uncurry assertMsg input) (isEq input))