{- CS 152 Homework 7:  Boba 2.0
   -}
module Boba2 where -- do not remove


data Token = OpenParen
           | CloseParen
           | Operator Char
           | PosNum Double
           | Let
           | Lambda
           | Identifier String
    deriving (Show, Eq)


data ExpTree = NumNode Double
             | OpNode Char [ExpTree]
             | IdentNode String
             | LetNode String ExpTree ExpTree
             | FunctionNode String ExpTree  -- one formal parameter only
             | Application ExpTree ExpTree -- First ExpTree is a FunctionNode
        deriving (Show, Eq)
        

-- STEP 1
keywords = ["let", "lambda"]

scan :: String ->[Token]
scan "" = []
scan xs = let [(token, lexRes)] = lex xs
          in case token of
          "(" -> OpenParen:scan lexRes
          ")" -> CloseParen:scan lexRes
          op |op `elem` ["+","-","*","/"] -> Operator (head op):scan lexRes
          keyw | keyw `elem` keywords -> scanHelper keyw:scan lexRes
          character -> let headChar = head character
                       in case headChar of
                       num | num `elem` ['0','1','2','3','4','5','6','7','8','9'] -> PosNum (read character :: Double):scan lexRes
                       op | op `elem`['+','-','*','/'] -> Operator op:scan ((tail character)++lexRes)
                       id | id `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['_'] -> Identifier character:scan lexRes
                       _ -> error ("Lexical Error - invalid character: " ++ (headChar:[]))

scanHelper :: String -> Token
scanHelper "let" = Let
scanHelper "lambda" = Lambda
scanHelper _ = error "Not a valid token"

-- STEP 2: uncomment each step as you work on it

parse :: [Token] -> ExpTree
parse xs = let (exptree, res) = expr xs
           in case (exptree,res) of
           (exptree,[]) -> exptree
           (_, res) -> error ("Parse error - extra tokens:"  ++ (show res))

-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN |  POSNUMBER
-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN
--      |    POSNUMBER
--      |    OPENPAREN LET IDENTIFIER <expr> <expr> CLOSEPAREN |
--      |    <application>
--      |    IDENTIFIER
expr :: [Token] -> (ExpTree, [Token])
expr (OpenParen:Operator x:rest) = let (ops, r1) = operands rest
                                   in case r1 of
                                   (OpenParen:PosNum x:r2) -> error ("Parse error: Invalid expression" ++ show(OpenParen:PosNum x:r2))
                                   (OpenParen:Operator x:r2) -> error ("Parse error: Invalid expression" ++ show r2)
                                   (CloseParen:r2) -> ((OpNode x)ops, r2)
                                   _ -> error ("Parse error: Invalid expression" ++ show r1)
expr (PosNum x:rest) = (NumNode x, rest)
expr (OpenParen:Let:Identifier x:rest) = let (exp, r1) = expr rest
                                         in case r1 of
                                         (OpenParen:r2) -> let (exp2,r3) = expr (OpenParen:r2)
                                                            in case r3 of
                                                            (CloseParen:r4) -> (LetNode x exp exp2, r4)
                                                            _ -> error ("invalid function application: " ++ show r3)
                                         xs -> error ("Parse Error: invalid function application: " ++ show xs)
expr (Identifier x:rest) = (IdentNode x, rest)
expr xs = application xs

-- <operands> ->  <expr> [<operands>]
operands :: [Token] -> ([ExpTree], [Token])
operands xs = let (exptree, res) = expr xs -- first call expr function
              in case (exptree,res) of
              (z, ys) -> let valid = operandsHelper ys
                         in case valid of
                         True -> let (ops, rest) = operands ys
                                 in (z:ops,rest)
                         False -> (z:[], ys)

operandsHelper :: [Token] -> Bool
operandsHelper (OpenParen:Operator t:CloseParen:r3) = False
operandsHelper (OpenParen:PosNum x:r3) = False
operandsHelper (OpenParen:r3) = True
operandsHelper (PosNum n:r3) = True
operandsHelper (Identifier x:r3) = True
operandsHelper (CloseParen:r3) = False
operandsHelper _ = False

-- <function> -> OPENPAREN LAMBDA OPENPAREN IDENTIFIER CLOSEPAREN <expr> CLOSEPAREN
function :: [Token] -> (ExpTree, [Token])
function (OpenParen:Lambda:OpenParen:Identifier x:CloseParen:rest) = let (y,r1) = expr rest
                                                                   in case (y,r1) of
                                                                   (_,(CloseParen:r2)) -> ((FunctionNode x)y,r2)
                                                                   (NumNode x,[]) -> error ("Invalid function: " ++ show (PosNum x :[]))
                                                                   (NumNode x,r2) -> error ("Invalid function: " ++ show (PosNum x :r2))
                                                                   _ -> error ("Invalid function:" ++ show r1)
function x = error ("Invalid function" ++ show x)

-- <application>	->  OPENPAREN <function> <expression> CLOSEPAREN
application :: [Token] -> (ExpTree, [Token])
application (OpenParen:rest) = let (x,r1) = function rest
                               in case r1 of
                               r2 -> let (y, r3) = expr r2
                                                  in case r3 of
                                                  (CloseParen:r4) -> (Application x y, r4)
                                                  _ -> error ("Invalid application" ++ show r3)
application x = error ("Invalid application" ++ show x)

stringToTree:: String -> ExpTree
stringToTree = parse.scan -- for testing convenience

--STEP 3: uncomment each step as you work on it
eval :: [(String, Double)] -> ExpTree -> Double
eval _ (NumNode x) = x
eval env (OpNode '+' (y:[])) = eval env y
eval env (OpNode '-' (y:[])) = 0 - eval env y
eval env (OpNode '*' (y:[])) = eval env y
eval env (OpNode '/' (y:[])) = 1 / eval env y
eval env (OpNode x (y:xs)) = case (x, y:xs) of
                     ('+',_) -> foldl (+) (eval env (y)) (map (eval env) (xs))
                     ('-',_) -> foldl (-) (eval env (y)) (map (eval env) (xs))
                     ('*',_) -> foldl (*) (eval env (y)) (map (eval env) (xs))
                     ('/',_) -> foldl (/) (eval env (y)) (map (eval env) (xs))
eval env (IdentNode x) = let val = lookup x env
                         in case val of
                         (Just z) -> z
                         Nothing -> error ("Undefined identifier: " ++ show x)
eval env (LetNode x exp1 exp2) = eval ((x, (eval env exp1)):env) exp2
eval env (Application (FunctionNode x exp1) exp2) = eval ((x, eval env exp2):env) exp1
eval _ x = error ("Evaluation error" ++ show x)

eval0 :: ExpTree -> Double
eval0 exptree = eval [] exptree

interpret :: String -> Double
interpret = eval0.parse.scan