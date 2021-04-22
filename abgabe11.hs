{-- Functional Pogramming
    SKI parser und eval Functions
    Original version: WS-08-09 Prof. Dr. Raul Rojas
    modified WS-09-10 by: Prof. Dr. Margarita Esponda
    modified WS-17-18 by: Prof. Dr. Margarita Esponda
    modified Exercise 11 at 14.02.2021: Bahareh Shidrang, Feliks Vdovichneko
--}

module SKII  where

-- (Expr, Show, parse, eval)
-- algebraic type for kombinator expresions

data Expr = App Expr Expr | S | K | I | Var String | Nil
              deriving Eq

instance Show Expr where show exp = show_expr exp True

show_expr :: Expr -> Bool-> String
show_expr S _ = "S"
show_expr K _ = "K"
show_expr I _ = "I"

show_expr (App x y) True  = "(App "++(show_expr x False)++(show_expr y True)++")"
show_expr (App x y) False = (show_expr x False)++(show_expr y True)

show_expr (Var x) _ = x


-- returns a list of free variables in an expression

freie::Expr->[String]->[String]
freie (Var x)   bound | elem x bound = []
                      | otherwise       = [x]
freie (App x y) bound = (freie x bound)++(freie y bound)
freie S bound = []
freie K bound = []
freie I bound = []

----------------------- Interpreter ----------------------------------

eval::Expr->Bool->Expr

eval S _ = S
eval K _ = K
eval I _ = I

eval (App I x) b = eval x b

eval (App (App K x) y) b = eval x b

eval (App (App (App S f) g) x) b = eval (App (App f x) (App g x)) b

eval (App x y) b
               | (evalx == x) && b = (App x y)
               | (evalx == x) && (not b)  = (App evalx (eval y False))
               | otherwise   = eval (App evalx y) b
                 where evalx = eval x True
eval (Var x) _ = (Var x)
eval x _ = error (show_expr x False)

----------------------------------------------------------------------

ski_parser :: String -> Expr
ski_parser exp = parse Nil exp

--- the parse function need to be defined hier ......
parse :: Expr -> String -> Expr
parse Nil  []                              = emptyExpr -- Error!
parse expr []                              = expr

parse Nil (a:rest)     | sletter a   = parse (Var [a]) rest
parse Nil [a]          | a=='S' || a=='K' || a=='I' = char2expr a
parse Nil (a:rest)     | a=='S' || a=='K' || a=='I' = parse (App (parse Nil expr1) (parse Nil expr2)) out
                                                     where (expr1, expr2, out) = findexpr (a:rest)
parse Nil ('(':rest)                       = parse (parse Nil inside) out
                                            where (inside, out) = extract [] rest 0

parse expr (a:rest)     | sletter a   = parse (App expr (Var [a])) rest
parse expr [a]          | a=='S' || a=='K' || a=='I' = App expr (char2expr a)
parse expr (a:rest)     | a=='S' || a=='K' || a=='I' = parse (App expr (App (parse Nil expr1) (parse Nil expr2))) out
                                                       where (expr1, expr2, out) = findexpr (a:rest)
parse expr ('(':rest)                       = parse (App expr (parse Nil inside)) out
                                              where (inside, out) = extract [] rest 0

parse _ rest = illegalExpr rest

---------------------------------- Help functions ------------------------------

char2expr x | x=='S' =S
            | x=='K' = K
            | x=='I' = I
            |otherwise =error "insert between these values" 

findexpr :: [Char] -> ([Char], [Char], [Char])
findexpr xs | '(' `elem` xs = (first1, second, out)
            | otherwise = (first2, [last2], [])
                        where 
                            (first1, second, out) = sepParan [] xs
                            (first2, last2)=findLast [] xs 


sepParan :: [Char] -> [Char] -> ([Char], [Char], [Char])
sepParan _ [] = error "error"
sepParan _ [x] = error "error"
sepParan first (x:xs) | x=='S' || x=='K' || x=='I' = sepParan (first++[x]) xs
                    | x=='(' = (first, inside, out)
                    |otherwise = error "error"
                                where (inside, out) = extract [] xs 0



findLast :: [Char] -> [Char] -> ([Char],Char)
findLast _ [] = error "error"
findLast first [x]  | x=='S' || x=='K' || x=='I' = (first, x)                    
findLast first  (x:xs) = findLast (first++[x]) xs

------------------------- Constant Error Functions ----------------------------

emptyExpr = error "the empty expression is not a valid lambda-Expression"
illegalExpr str = error ("there is a syntax error in the expression  " ++ str)


--------------------- Auxiliary Functions -----------------------

sletter x = elem x ['a'..'z']

extract a   []     _  = error "unbalanced parentheses"
extract a (')':b)  0  = (a,b)
extract a (')':b)  n  = extract  (a++")")  b (n-1)
extract a ('(':b)  n  = extract  (a++"(")  b (n+1)
extract a (b:c)    n  = extract  (a++[b])  c  n

--------------- some SKI expresions for testing -----------------------

expand "succ" = "(S(S(KS)K))"
expand "T" =  "I"
expand "F" = "(KI)"
expand "pred" = "(S(S(SI(K(S(S(KS)(S(K(SI))(S(KK)(S(K(S(S(KS)K)))(SI(KK))))))(S(KK)(SI(KK))))))(K(S(SI(K(KI)))(K(KI)))))(K(KI)))"
expand "or" = "(SI(KK))"
expand "and" = "(SS(K(K(KI))))"
expand "*" = "(S(KS)K)"
-- usw.

--------------------------------------------------------------------------
-----------       SKI Parser and Interpreter       -----------------------

skii exp = show_expr (eval (ski_parser exp) False) False
--------------------------------------------------------------------------
--------------------------------------------------------------------------



test_1_1 = skii "(S(SI(K(KI)))(K(S(KK)I)))(S(KK)I)"
test_1_2 = skii "(S(S(KS)(S(S(KS)(S(KK)I))(KI)))(K(K(KI))))(S(KK)I)(KI)"