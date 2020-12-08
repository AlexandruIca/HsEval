module TokenizerTest(
    tokenizerTests,
) where

import Test.Tasty
import Test.Tasty.HUnit

import Tokenizer

tests :: [(String, [Token])]
tests =
    [
    ("(", [RParen]),
    ("", []),
    ("  ", []),
    ("+", [Plus]),
    ("-", [Minus]),
    ("/", [Div]),
    ("*", [Mult]),
    ("^", [Pow]),
    ("(", [LParen]),
    (")", [RParen]),
    ("2", [Number 2.0]),
    ("1+2", [Number 1.0, Plus, Number 2.0]),
    ("10^2", [Number 10.0, Pow, Number 2.0]),
    ("2^(3+7)", [Number 2.0, Pow, LParen, Number 3.0, Plus, Number 7.0, RParen]),
    ("2 ^ ( 3 + 7    )", [Number 2.0, Pow, LParen, Number 3.0, Plus, Number 7.0, RParen]),
    ("1 + 256 - 3 / 4 * 5 + 5^(2^1)", [ Number 1.0, Plus, Number 256.0, Minus
                                      , Number 3.0, Div, Number 4.0, Mult, Number 5.0, Plus
                                      , Number 5.0, Pow, LParen, Number 2.0, Pow, Number 1.0, RParen
                                      ])
    ]

tokenizerTests :: [TestTree]
tokenizerTests = map toTestTree tests
    where
        isEq :: (String, [Token]) -> Bool
        isEq (s, tokens) = and $ map (\(a, b) -> a == b) (zip (tokenize s) tokens)

        assertMsg :: String -> String
        assertMsg s = "`tokenize` failed for '" ++ s ++ "', output: " ++ show (tokenize s)

        toTestTree :: (String, [Token]) -> TestTree
        toTestTree input = testCase "" (assertBool (assertMsg $ fst input) (isEq input))
