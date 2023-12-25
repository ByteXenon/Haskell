data Token
  = Identifier String
  | Number Integer
  | Operator String
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isWhitespace c = lexer cs
  | isDigit c = let (digits, rest) = span isDigit (c:cs)
                in Number (read digits) : lexer rest
  | isAlpha c = let (ident, rest) = span isAlphaNum (c:cs)
                in Identifier ident : lexer rest
  | isOperator c = let (op, rest) = span isOperator (c:cs)
                   in Operator op : lexer rest
  | otherwise = lexer cs

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n']

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

isAlpha :: Char -> Bool
isAlpha c = c `elem` (['a'..'z'] ++ ['A'..'Z'])

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isOperator :: Char -> Bool
isOperator c = c `elem` ['+', '-', '*', '/']

evaluate :: [Token] -> Integer
evaluate tokens = evalExpr tokens []

evalExpr :: [Token] -> [Integer] -> Integer
evalExpr [] [result] = result
evalExpr (Number n : tokens) stack = evalExpr tokens (n : stack)
evalExpr (Operator op : tokens) (y : x : stack) = evalExpr tokens (applyOp op x y : stack)
evalExpr _ _ = error "Invalid expression"

applyOp :: String -> Integer -> Integer -> Integer
applyOp "+" x y = x + y
applyOp "-" x y = x - y
applyOp "*" x y = x * y
applyOp "/" x y = x `div` y

main :: IO ()
main = do
  let input = "2 + 3"
  let tokens = lexer input
  let result = evaluate tokens
  print result
