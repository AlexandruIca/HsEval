module RecursiveDescent
  ( calc,
  )
where

import Data.Char (isAlpha, isDigit, ord)

data Token
  = Number Double
  | LParen
  | RParen
  | Plus
  | Minus
  | Mult
  | Div
  | Ident String
  | End
  deriving (Show)

instance Eq Token where
  LParen == LParen = True
  RParen == RParen = True
  Plus == Plus = True
  Minus == Minus = True
  Mult == Mult = True
  Div == Div = True
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

    isWhitespace :: Char -> Bool
    isWhitespace c = c `elem` " \t\r\n"

    parseInt :: Double -> String -> (Double, String)
    parseInt currentNumber [] = (currentNumber, [])
    parseInt currentNumber (next : peek)
      | isDigit next = parseInt (currentNumber * 10 + fromIntegral (ord next - ord '0')) peek
      | otherwise = (currentNumber, next : peek)

    parseIdent :: String -> String -> (String, String)
    parseIdent currentIdent [] = (currentIdent, [])
    parseIdent currentIdent (next : peek)
      | isAlpha next = parseIdent (currentIdent ++ [next]) peek
      | otherwise = (currentIdent, next : peek)

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
      | isWhitespace current = consumeInput tok next
      | isAlpha current =
        let (ident, rest) = parseIdent [current] next
         in consumeInput (Ident ident : tok) rest
      | otherwise = Right $ "Unexpected token: '" ++ [current] ++ "'"

data Operator = PlusOp | MinusOp | MultOp | DivOp
  deriving (Show, Eq)

data Function = Sin | Cos | Tan | Asin | Acos | Atan | Exp | Log | Sqrt
  deriving (Show, Eq)

data AST
  = SumNode Operator AST AST
  | ProdNode Operator AST AST
  | FnNode Function AST
  | NumNode Double
  deriving (Show)

type ASTResult = Either (AST, [Token]) String

peek :: [Token] -> Token
peek [] = End
peek (t : _) = t

consume :: [Token] -> [Token]
consume [] = []
consume (_ : ts) = ts

expression :: [Token] -> ASTResult
expression toks = expression' $ term toks
  where
    go :: AST -> Operator -> ASTResult -> ASTResult
    go _ _ (Right str) = Right str
    go termTree op (Left (expTree, toks'')) =
      Left (SumNode op termTree expTree, toks'')

    expression' :: ASTResult -> ASTResult
    expression' (Right str) = Right str
    expression' (Left (termTree, toks')) =
      case peek toks' of
        Plus -> go termTree PlusOp (expression $ consume toks')
        Minus -> go termTree MinusOp (expression $ consume toks')
        _ -> Left (termTree, toks')

term :: [Token] -> ASTResult
term toks = term' $ factor toks
  where
    go :: AST -> Operator -> ASTResult -> ASTResult
    go _ _ (Right str) = Right str
    go facTree op (Left (termTree, toks'')) =
      Left (ProdNode op facTree termTree, toks'')

    term' :: ASTResult -> ASTResult
    term' (Right str) = Right str
    term' (Left (facTree, toks')) =
      case peek toks' of
        Mult -> go facTree MultOp (term $ consume toks')
        Div -> go facTree DivOp (term $ consume toks')
        _ -> Left (facTree, toks')

factor :: [Token] -> Either (AST, [Token]) String
factor toks =
  case peek toks of
    (Number x) -> Left (NumNode x, consume toks)
    (Ident str) -> chooseFn str (factor $ consume toks)
    LParen -> expectRParen $ expression (consume toks)
    End -> Left (NumNode 0.0, [])
    t -> Right $ "Parse error on token curr=" ++ show t ++ " rest=" ++ show toks
  where
    matchFn :: String -> Either Function String
    matchFn "sin" = Left Sin
    matchFn "cos" = Left Cos
    matchFn "tan" = Left Tan
    matchFn "asin" = Left Asin
    matchFn "acos" = Left Acos
    matchFn "atan" = Left Atan
    matchFn "log" = Left Log
    matchFn "exp" = Left Exp
    matchFn "sqrt" = Left Sqrt
    matchFn s = Right $ "Unknown function '" ++ s ++ "'"

    chooseFn :: String -> ASTResult -> ASTResult
    chooseFn _ (Right str) = Right str
    chooseFn fn (Left (ast, toks')) = chooseFn' ast toks' (matchFn fn)
      where
        chooseFn' :: AST -> [Token] -> Either Function String -> ASTResult
        chooseFn' _ _ (Right err) = Right err
        chooseFn' ast' toks'' (Left fn') = Left (FnNode fn' ast, toks'')

    expectRParen :: ASTResult -> ASTResult
    expectRParen (Right str) = Right str
    expectRParen (Left (expTree, toks')) =
      if peek toks' /= RParen
        then Right "Missing ')'"
        else Left (expTree, consume toks')

parse :: TokenizerResult -> ASTResult
parse (Right err) = Right err
parse (Left [End]) = Left (NumNode 0.0, [])
parse (Left toks) = parse' $ expression toks
  where
    parse' :: ASTResult -> ASTResult
    parse' (Right str) = Right str
    parse' (Left (tree, toks')) =
      if null toks'
        then Left (tree, [])
        else Right $ "Leftover: " ++ show toks'

eval :: ASTResult -> Either Double String
eval (Right err) = Right err
eval (Left (ast, _)) = Left (eval' ast)
  where
    eval' :: AST -> Double
    eval' (NumNode n) = n
    eval' (SumNode op lhs rhs)
      | op == PlusOp = eval' lhs + eval' rhs
      | op == MinusOp = eval' lhs - eval' rhs
    eval' (ProdNode op lhs rhs)
      | op == MultOp = eval' lhs * eval' rhs
      | op == DivOp = eval' lhs / eval' rhs
    eval' (FnNode fn ast)
      | fn == Sin = sin (eval' ast)
      | fn == Cos = cos (eval' ast)
      | fn == Tan = tan (eval' ast)
      | fn == Asin = asin (eval' ast)
      | fn == Acos = acos (eval' ast)
      | fn == Atan = atan (eval' ast)
      | fn == Exp = exp (eval' ast)
      | fn == Log = log (eval' ast)
      | fn == Sqrt = sqrt (eval' ast)

calc :: String -> Either Double String
calc = eval . parse . tokenize