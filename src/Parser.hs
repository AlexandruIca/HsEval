module Parser
  ( AST (ASTNumber, ASTOp),
    rpn,
    interpret,
    evalPostfix,
    RPNResult,
    InterpreterResult,
  )
where

import Tokenizer

data AST
  = ASTNumber Double
  | ASTOp AST AST Token

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _ = False

isOperator :: Token -> Bool
isOperator Plus = True
isOperator Minus = True
isOperator Mult = True
isOperator Div = True
isOperator Pow = True
isOperator _ = False

isLeftAssoc :: Token -> Bool
isLeftAssoc Plus = True
isLeftAssoc Minus = True
isLeftAssoc Mult = True
isLeftAssoc Div = True
isLeftAssoc _ = False

isRightAssoc :: Token -> Bool
isRightAssoc Pow = True
isRightAssoc _ = False

prec :: Token -> Int
prec Plus = 2
prec Minus = 2
prec Mult = 3
prec Div = 3
prec Pow = 4
prec _ = 0

isLParen :: Token -> Bool
isLParen LParen = True
isLParen _ = False

isRParen :: Token -> Bool
isRParen RParen = True
isRParen _ = False

type Stack = [Token]

type RPNResult = TokenizerResult

shuntingYard :: Stack -> Stack -> Stack -> Stack
shuntingYard [] [] result = result
shuntingYard [] (op : ops) result = shuntingYard [] ops (op : result)
shuntingYard (tok : toks) operators result
  | isNumber tok = shuntingYard toks operators (tok : result)
  | isOperator tok = case operators of
    [] -> shuntingYard toks (tok : operators) result
    (op : ops) ->
      if (isLeftAssoc tok && (prec tok <= prec op)) || (isRightAssoc tok && (prec tok < prec op))
        then shuntingYard (tok : toks) ops (op : result)
        else shuntingYard toks (tok : operators) result
  | isLParen tok = shuntingYard toks (tok : operators) result
  | isRParen tok = case operators of
    (LParen : ops) -> shuntingYard toks ops result
    (op : ops) -> shuntingYard (tok : toks) ops (op : result)

-- Reverse Polish Notation
rpn :: TokenizerResult -> RPNResult
rpn (Left tokens) = Left $ reverse $ shuntingYard tokens [] []
rpn (Right err) = Right err

type InterpreterResult = Either Double String

interpret :: RPNResult -> InterpreterResult
interpret (Right err) = Right err
interpret (Left tokens) = interpret' [] tokens 0
  where
    takeAllButLast2 :: [a] -> [a]
    takeAllButLast2 lst = take (length lst - 2) lst

    takeLast2 :: [a] -> (a, a)
    takeLast2 lst =
      let toConvert = reverse $ take 2 (reverse lst)
       in (head toConvert, last toConvert)

    processOperation :: Token -> (Double, Double) -> Double
    processOperation Plus (a, b) = a + b
    processOperation Minus (a, b) = a - b
    processOperation Mult (a, b) = a * b
    processOperation Div (a, b) = a / b
    processOperation Pow (a, b) = a ** b
    processOperation _ _ = error "Unexpected token in `processOperation` when interpreting"

    interpret' :: [Double] -> Stack -> Double -> InterpreterResult
    interpret' [] [] x = Left x
    interpret' (x : _) [] _ = Left x
    interpret' acc (tok : toks) result = case tok of
      Number n -> interpret' (acc ++ [n]) toks result
      LParen -> Right "Unexpected '(' token in `interpret'`"
      RParen -> Right "Unexpected ')' token in `interpret'`"
      _ ->
        let res = processOperation tok (takeLast2 acc)
         in interpret' (takeAllButLast2 acc ++ [res]) toks res

evalPostfix :: String -> InterpreterResult
evalPostfix expression = interpret (rpn . tokenize $ expression)
