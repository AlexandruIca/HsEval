module RPNTest
  ( rpnTests,
  )
where

import Parser
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase)
import Tokenizer

tests :: [(String, [Token])]
tests =
  [ ("", []),
    ("   ", []),
    ("\n\t\r ", []),
    ("100", [Number 100.0]),
    ("1+2", [Number 1.0, Number 2.0, Plus]),
    ("3 * 4 / 2 ^ 7", [Number 3.0, Number 4.0, Mult, Number 2.0, Number 7.0, Pow, Div]),
    ("(3 * 4 / 2) ^ 7", [Number 3.0, Number 4.0, Mult, Number 2.0, Div, Number 7.0, Pow]),
    ("2 ^ 4 ^ 3", [Number 2.0, Number 4.0, Number 3.0, Pow, Pow]),
    ("2 * 4 * 3", [Number 2.0, Number 4.0, Mult, Number 3.0, Mult]),
    ("2 ^ 4 * 3", [Number 2.0, Number 4.0, Pow, Number 3.0, Mult]),
    ("2 ^ 4 - 3", [Number 2.0, Number 4.0, Pow, Number 3.0, Minus])
  ]

rpnTests :: [TestTree]
rpnTests = map toTestTree tests
  where
    filterInput :: String -> String
    filterInput s = filter (`notElem` " \n\r\t") s

    makeResult :: RPNResult -> [Token] -> [(Token, Token)]
    makeResult (Left tokens) expectedTokens = zip tokens expectedTokens
    makeResult (Right _) _ = [(Number 0.0, Pow)]

    isEq :: (String, [Token]) -> Bool
    isEq (s, tokens) = all (uncurry (==)) (makeResult (rpn . tokenize $ s) tokens)

    assertMsg :: String -> [Token] -> String
    assertMsg s expected =
      "`rpn` failed for '"
        ++ s
        ++ "'\n\t\t expected: "
        ++ show expected
        ++ "\n\t\t output: "
        ++ show (rpn . tokenize $ s)

    toTestTree :: (String, [Token]) -> TestTree
    toTestTree input =
      testCase
        ("RPN[" ++ filterInput (fst input) ++ "]")
        (assertBool (uncurry assertMsg input) (isEq input))