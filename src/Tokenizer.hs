module Tokenizer
  ( Token (Number, LParen, RParen, Plus, Minus, Mult, Div, Pow),
    tokenize,
    TokenizerResult,
  )
where

import Data.Char (ord)

data Token
  = Number Double
  | LParen
  | RParen
  | Plus
  | Minus
  | Mult
  | Div
  | Pow
  deriving (Show)

instance Eq Token where
  LParen == LParen = True
  RParen == RParen = True
  Plus == Plus = True
  Minus == Minus = True
  Mult == Mult = True
  Div == Div = True
  Pow == Pow = True
  (Number _) == (Number _) = True
  _ == _ = False

type ErrorMessage = String

type TokenizerResult = Either [Token] ErrorMessage

tokenize :: String -> TokenizerResult
tokenize expr = rev (consumeInput [] expr)
  where
    rev :: TokenizerResult -> TokenizerResult
    rev (Left tokens) = Left $ reverse tokens
    rev (Right err) = Right err

    isDigit :: Char -> Bool
    isDigit c = c `elem` "0123456789"

    isWhitespace :: Char -> Bool
    isWhitespace c = c `elem` " \t\r\n"

    parseInt :: Double -> String -> (Double, String)
    parseInt currentNumber [] = (currentNumber, [])
    parseInt currentNumber (next : peek)
      | isDigit next = parseInt (currentNumber * 10 + fromIntegral (ord next - ord '0')) peek
      | otherwise = (currentNumber, next : peek)

    consumeInput :: [Token] -> String -> TokenizerResult
    consumeInput tok [] = Left tok
    consumeInput tok (current : next)
      | isDigit current =
        let (num, rest) = parseInt (read [current] :: Double) next
         in consumeInput (Number num : tok) rest
      | current == '(' = consumeInput (LParen : tok) next
      | current == ')' = consumeInput (RParen : tok) next
      | current == '+' = consumeInput (Plus : tok) next
      | current == '-' = consumeInput (Minus : tok) next
      | current == '*' = consumeInput (Mult : tok) next
      | current == '/' = consumeInput (Div : tok) next
      | current == '^' = consumeInput (Pow : tok) next
      | isWhitespace current = consumeInput tok next
      | otherwise = Right $ "Unexpected token: '" ++ [current] ++ "'"
