module RecursiveDescentTest
  ( calcTests,
  )
where

import RecursiveDescent
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase)

tests :: [(String, Double)]
tests =
  [ ("", 0.0),
    ("   ", 0.0),
    ("\n\t\r ", 0.0),
    ("100", 100.0),
    ("1+2", 3.0),
    ("3 * 4 / 2 - 7", -1.0),
    ("(3 * 4 / 2)", 6.0),
    ("sin(5 - 5)", 0.0),
    ("sin(asin(0))", 0.0),
    ("log(exp(1))", 1.0),
    ("cos(acos(0))", 0.0),
    ("tan(atan(0))", 0.0),
    ("sqrt(2 * 2 * log(exp(1))) - 1", 1.0)
  ]

calcTests :: [TestTree]
calcTests = map toTestTree tests
  where
    filterInput :: String -> String
    filterInput s = filter (`notElem` " \n\r\t") s

    makeResult :: Either Double String -> Double -> (Int, Int)
    makeResult (Left result) expectedResult = (round result, round expectedResult)
    makeResult (Right _) _ = (1, 2)

    isEq :: (String, Double) -> Bool
    isEq (s, result) = uncurry (==) (makeResult (calc s) result)

    assertMsg :: String -> Double -> String
    assertMsg s expected =
      "`calc` failed for '"
        ++ s
        ++ "'\n\t\t expected: "
        ++ show expected
        ++ "\n\t\t output: "
        ++ show (calc s)

    toTestTree :: (String, Double) -> TestTree
    toTestTree input =
      testCase
        ("RecursiveDescentTest[" ++ filterInput (fst input) ++ "]")
        (assertBool (uncurry assertMsg input) (isEq input))