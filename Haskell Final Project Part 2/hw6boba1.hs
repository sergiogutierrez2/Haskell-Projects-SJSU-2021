{- CS 152 Homework 6:  Boba 1.0
   -}
module Boba1 where -- do not remove

data Token = OpenParen
           | CloseParen
           | Operator Char
           | PosNum Double
    deriving (Show, Eq)

data ExpTree = NumNode Double
             | OpNode Char [ExpTree]
        deriving Show

-- STEP 1
scan :: String ->[Token]
scan "" = []
scan xs = let [(token, lexRes)] = lex (stringProcess ' ' xs)
          in case token of
          "(" -> OpenParen:scan lexRes
          ")" -> CloseParen:scan lexRes
          "+" -> Operator '+':scan lexRes
          "-" -> Operator '-':scan lexRes
          "*" -> Operator '*':scan lexRes
          "/" -> Operator '/':scan lexRes
          character -> let headChar = head character
                       in case headChar of
                       '0' -> PosNum (read character :: Double):scan lexRes
                       '1' -> PosNum (read character :: Double):scan lexRes
                       '2' -> PosNum (read character :: Double):scan lexRes
                       '3' -> PosNum (read character :: Double):scan lexRes
                       '4' -> PosNum (read character :: Double):scan lexRes
                       '5' -> PosNum (read character :: Double):scan lexRes
                       '6' -> PosNum (read character :: Double):scan lexRes
                       '7' -> PosNum (read character :: Double):scan lexRes
                       '8' -> PosNum (read character :: Double):scan lexRes
                       '9' -> PosNum (read character :: Double):scan lexRes
                       x -> error ("Lexical Error - invalid character: " ++ (x:[]))

-- Proces the string to include whitespaces between characters
stringProcess :: Char -> [Char] -> [Char]
stringProcess _ [] = []
stringProcess y (x:xs)
    |x `elem` ['+','-','*','/'] = x:y:(stringProcess y xs)
    |otherwise = x:(stringProcess y xs)

-- STEP 2: uncomment each step as you work on it

parse :: [Token] -> ExpTree
parse xs = let (exptree, res) = expr xs
           in case (exptree,res) of
           (exptree,[]) -> exptree
           (_, res) -> error ("Parse error - extra tokens:"  ++ (show res))

-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN |POSNUMBER
expr :: [Token] -> (ExpTree, [Token])
expr (OpenParen:Operator x:rest) = let (ops, r1) = operands rest
                                   in case r1 of
                                   (CloseParen:r2) -> ((OpNode x)ops, r2)
expr (PosNum x:rest) = (NumNode x, rest)
expr (OpenParen:PosNum x:rest) = error ("Parse error - Invalid expression" ++ show(OpenParen:PosNum x:rest))
expr (CloseParen:PosNum x:rest)= error ("Parse error - Invalid expression" ++ show(CloseParen:PosNum x:rest))
expr (OpenParen:xs) = error ("Parse error - Invalid expression" ++ show(OpenParen:xs))
expr _ = error "Parse error - Invalid expression"

-- <operands> -> <expr> [<operands>]
operands :: [Token] -> ([ExpTree], [Token])
operands xs = let (exptree, res) = expr xs -- first call expr function
              in case (exptree,res) of
              (NumNode w, (x:ys)) -> case x of
                                     OpenParen -> let (ops, rest) = operands (x:ys)
                                                  in ((NumNode w:ops,rest))
                                     PosNum z -> let (ops, rest) = operands (x:ys)
                                                 in ((NumNode w:ops),rest)
                                     CloseParen -> (NumNode w:[], (x:ys))
              (z, (x:ys)) -> case x of
                             OpenParen -> let (ops, rest) = operands (x:ys)
                                          in (z:ops,rest)
                             PosNum n -> let (ops, rest) = operands (x:ys)
                                         in (z:ops,rest)
                             _ -> (z:[], (x:ys))

stringToTree:: String -> ExpTree
stringToTree = parse.scan -- for testing convenience


-- STEP 3: uncomment each step as you work on it

eval ::  ExpTree -> Double
eval (NumNode x) = x
eval (OpNode x (y:[])) = case x of
                         '+' -> eval y
                         '-' -> 0 - eval y
                         '*' -> eval y
                         '/' -> 1 / eval y
eval (OpNode x (y:xs)) = case (x, y:xs) of
                     ('+',_) -> foldl (+) (eval (y)) (map eval (xs))
                     ('-',_) -> foldl (-) (eval (y)) (map eval (xs))
                     ('*',_) -> foldl (*) (eval (y)) (map eval (xs))
                     ('/',_) -> foldl (/) (eval (y)) (map eval (xs))

--interpret :: String -> Double
interpret = eval.parse.scan
