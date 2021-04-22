--- Lambda Calculus Parser and Interpreter
--- Original version: WS-97-98 Prof. Dr. Raul Rojas
--- modified WS-09-10 by: Prof. Dr. Margarita Esponda
--- modified SoSe-2017 by: Prof. Dr. Margarita Esponda
--- modified and added functions WiSe-2020 by: Bahareh Shidrang


module Lambda_Calcul (Expr, parser, eval, show)  where

-- Definition of an algebraic type for lambda expressions
-- data Expr = Var String | App Expr Expr | Lambda String Expr | Nil deriving Eq

data Expr = Var String | App Expr Expr | Lambda String Expr | Nil  deriving Eq
type Parser = String -> Expr

instance Show Expr where show = expr2string

------------------ show function of lambda-expressions ----------------

expr2string::Expr->String
expr2string (Var x)   =  x
expr2string (App x y) = "(" ++ (expr2string x) ++ (expr2string y) ++")"
expr2string (Lambda x y) = "(/" ++ x ++ "." ++ (expr2string y) ++")"

------------------------------ PARSER Version 4 -------------------------------------
parser :: Parser
parser str = parse Nil str

parse :: Expr -> String -> Expr
parse Nil  []                              = emptyExpr -- Error!
parse expr []                              = expr

parse Nil (a:rest)         | lowLetter a   = parse (Var [a]) rest
parse Nil ('/':a:'.':rest) | lowLetter a   = Lambda [a] (parse Nil rest)
parse Nil ('/':a:b:rest)   | (lowLetter a)
                          && (lowLetter b) = Lambda [a] (parse Nil ('/':b:rest))

parse Nil ('(':rest)                       = parse (parse Nil inside) out
                                              where (inside, out) = extract [] 0 rest

parse expr (a:rest)        | lowLetter a   = parse (App expr (Var [a])) rest
parse expr ('{':rest)                      = parse expr ((expandExpr a)++(tail b))
                                               where (a, b) = (break (=='}') rest)

parse expr ('/':a:'.':rest) | lowLetter a   = App expr (Lambda [a] (parse Nil rest))
parse expr ('/':a:b:rest)   | (lowLetter a)
                           && (lowLetter b) = App expr (Lambda [a] (parse Nil ('/':b:rest)))
parse expr ('(':rest)                       = parse (App expr (parse Nil inside)) out
                                              where (inside, out) = extract [] 0 rest

parse expr rest = illegalExpr rest

---------------------------------- Help functions ------------------------------

lowLetter x = elem x ['a'..'z']
isDigit x = elem x ['0'..'9']

extract :: String -> Int -> String -> (String, String)
extract a  _  []      = error "unbalanced parentheses"
extract a  0  (')':rest) = (a,rest)
extract a  n  (')':rest) = extract  (a++")")  (n-1) rest
extract a  n  ('(':rest) = extract  (a++"(")  (n+1) rest
extract a  n  (b:rest)   = extract  (a++[b])   n   rest

expandExpr :: [Char] -> [Char]
expandExpr str | isNum  str = int2lambda (string2Num str)
               | otherwise  = expand str

int2lambda :: Int -> String
int2lambda 0 = "(/s./z.z)"
int2lambda n = "(/s./z."++ (foldr (++) [] (take n (repeat "s("))) ++ "z" ++ (take n [')',')'..]) ++ ")"

isNum :: String -> Bool
isNum xs = all isDigit xs

char2Digit :: Char -> Int
char2Digit x | isDigit x   = (fromEnum x) - (fromEnum '0')
             | otherwise   =  error  "wrong digit character ..."

string2Num :: String -> Int
string2Num str = str2Num (reverse str)
                 where
                 str2Num []     = 0
                 str2Num (z:zs) = 10*(string2Num zs) + (char2Digit z)

------------------------- Constant Error Functions ----------------------------

emptyExpr = error "the empty expression is not a valid lambda-Expression"
notAnumber = error "an empty string is not a number"
illegalExpr str = error ("there is a syntax error in the expression  " ++ str)

---------------------- macro functions defined in the lecture ------------

-- arithmetic and logic functions

expand "S"  = "(/w./x./y.x(wxy))"   -- succesor
expand "+"  = "(/x./y.x{S}y)"       -- addition
expand "*"  = "(/x./y./z.x(yz))"    -- multiplication
expand "T"  = "(/a./b.a)"           -- true
expand "F"  = "(/a./b.b)"           -- false
expand "&"  = "(/x./y.xy{F})"       -- and
expand "|"  = "(/x./y.x{T}y)"       -- or
expand "NOT"= "(/x.x{F}{T})"        -- not
expand "Z"  = "(/x.x{F}{NOT}{F})"   -- compare to 0
expand "GT" = "(/x./y.{Z}(x{P}y))"  -- greater as
expand "="  = "(/x./y.{&}({GT}xy)({GT}yx))" -- equal

-- predecesor function

expand "P" = "(/n.n{H}(/z.z{0}{0}){F})" -- predecessor
expand "H" = "(/pz.z({S}(p{T}))(p{T}))" -- (n,n-1) to (n,n+1)

-------- recursion operator for call-by-name ----------

expand "Y" = "(/r.(/x.r(xx))(/x.r(xx)))"

-------- macros with long names

expand "FIB" =  "(/rn.{=}n{0}{0}({=}n{1}{1}({+}(r({P}n))(r({P}({P}n))))))" -- fibonacci
expand "SUM" =  "(/rn.{Z}n{0}({+}n(r({P}n))))"                             -- recursive sum

-- more functions for lists

expand "NIL"  = "(/x.x(/abc.a))"                          -- test nil
expand "TAIL" = "(/x.x(/abc.c))"                          -- tail
expand "HEAD" = "(/x.x(/abc.b))"                          -- head

expand "LEN"  = "(/rl.{NIL}l{0}({S}(r({TAIL}l))))"        -- length

-- some lists for testing

expand "L1" = "(/z.z{F}{1}{L})"
expand "L2" = "(/z.z{F}{2}{L1})"
expand "L3" = "(/z.zF3{L2})"

-- integer numbers

expand "0" = "(/z.z{0}{0})"                          -- (0,0)
expand "1" = "(/z.z{0}{1})"                          -- (1,0)
expand "2" = "(/z.z{0}{2})"                          -- (2,0)
expand "-1" = "(/z.z{1}{0})"                         -- (0,1)
expand "-2" = "(/z.z{2}{0})"                         -- (0,2)

expand "ZADD" = "(/ab.(/z.z(+(a{T})(b{T}))(+(a{F})(b{F}))))" -- add (a,b) (c,d) = (a+c,b+d)
expand "Zadd" = "(/ab.(/z.z((a{T}){S}(b{T}))((a{F}){S}(b{F}))))"

--Exercise 10
expand "**" = "(/x./y.{Z}y{1}(({P}y)({*}{x}){x}))"       -- Power
expand "3**" = "(/y.{Z}y{1}(({P}y)({*}{3}){3}))"


-------------------------------------------------------------------------------------------

------------------------ Lambda Interpreter (Version 1) -----------------------------------

eval0 :: Expr->Expr
eval0 (Var x)                = Var x
eval0 (Lambda x exp)            = Lambda x exp
eval0 (App  (Var x)  exp)    = App (Var x) (eval0 exp)
eval0 (App (Lambda a e1) e2)    = eval0 (subst a e2 e1)
eval0 (App (App e1 e2) e3)
           | (evale12 == (App e1 e2))   = App (App e1 e2) (eval0 e3)
           | otherwise                  = eval0 (App evale12 e3)
                                          where evale12 = eval0 (App e1 e2)

------------------------ Lambda Interpreter (Version 2) -----------------------------------

eval :: Expr -> Expr
eval = evalB False
evalB :: Bool -> Expr -> Expr
evalB  _   (Var x)            = Var x
evalB  False  (Lambda x exp)     = Lambda x (evalB False exp)
evalB  True   (Lambda x exp)     = Lambda x exp
evalB  b  (App  e1 e2)
           | is_lambda eval_e1   = evalB b (subst (getarg eval_e1) e2 (getbody eval_e1))
           | otherwise           = (App eval_e1 (evalB False e2))
                                 where eval_e1 = evalB True e1
                                       getarg  (Lambda x body)  = x
                                       getbody (Lambda x body)  = body

-------------------- Auxiliary Functions for the interpreter ---------------------------

is_lambda::Expr->Bool
is_lambda (Lambda a b) = True
is_lambda  x           = False

-- returns the list of free variables of an expression ---------

freeList::Expr->[String]->[String]
freeList (Var x)  bound | elem x bound = []
                        | otherwise    = [x]
freeList (Lambda x y) bound            = freeList y (x:bound)
freeList (App x y)    bound            = (freeList x bound) ++ (freeList y bound)

-- returns the list of bounded variables in an expression ------

boundList::Expr->[String]->[String]
boundList (Var x)      bound = []
boundList (Lambda x y) bound = x:(boundList y (x:bound))
boundList (App x y)    bound = (boundList x bound) ++ (boundList y bound)

boundList2::Expr -> [String]
boundList2 (Var x)       = []
boundList2 (Lambda x y)  = x:(boundList2 y)
boundList2 (App x y)     = (boundList2 x) ++ (boundList2 y)

-- finds a name which is not in the list of prohibited names ----

find_new_name::[String]->String
find_new_name prohibited = head [[u]|u<-['a'..'z'], not( elem [u] prohibited ) ]

-- renames variable y with the new name n in the expression -----------------

rename::String->String->Expr->Expr
rename y nn (Var x)  | x==y      = Var nn
                     | otherwise = Var x
rename y nn (Lambda x z)
                   | x==y      = (Lambda nn (rename y nn z))
                   | otherwise = (Lambda x (rename y nn z))
rename y nn (App x z) = (App (rename y nn x) (rename y nn z))

-- substitute variable (x) with expression (exp) in the expression of the
-- third argument, taking into account the free variables in exp

subst::String->Expr->Expr->Expr
subst x exp (Var y)  | x==y      =  exp
                     | otherwise = Var y
subst x exp (Lambda y z)
           | x==y                       =  Lambda y z
           | (elem y (freeList exp [])) = subst x exp (rename y newname (Lambda y z))
           | otherwise                  = (Lambda y (subst x exp z))
                where newname = find_new_name ((freeList exp [])++(freeList z [])++(boundList2 z))
subst x exp (App y z) = App (subst x exp y) (subst x exp z)


-----------------------------------------------------------------------------------

---------- lci is the complete lambda-calcul interpreter --------------------------
---------- parse, evaluate and show a lambda expression ---------------------------

-----------------------------------------------------------------------------------

lci :: String -> Expr
lci = eval.parser

lci0 :: String -> Expr
lci0 = eval0.parser

--- Examples -----------

test1 = lci "{15}"
test2 = lci "{Y}{FIB}{6}"
test3 = lci "{Y}{SUM}{6}"



-- Aufgabe 1
{-I defined the power function at line 147
expand "**" = "(/x./y.{Z}y{1}(({P}y)({*}{x}){x}))"       -- Power
expand "3**" = "(/y.{Z}y{1}(({P}y)({*}{3}){3}))"
-}



test_aufgabe1a_0 = lci "{3**}{0}"
test_aufgabe1a_1 = lci "{3**}{1}"
test_aufgabe1a_2 = lci "{3**}{2}"
test_aufgabe1a_3 = lci "{3**}{3}"


-- Aufgabe 1.b
rev_list list = foldl (\xs x-> x:xs) [] list

rev_list_test = rev_list [3,5,9,8]


-- Aufgabe 2
fix re = re (fix re)



collatz = (\r n -> if n<1 then error "insert bigger than zero" else (if n==4 then n else (if (mod n 2 ==0) then r (div n 2) else r ((n*3)+1) )))

test_collatz_0 = (fix collatz) 10
test_collatz_1 = (fix collatz) 7
test_collatz_2 = (fix collatz) 2687
test_collatz_3 = (fix collatz) 957



--Aufgabe 3
lengthL list = foldl (\s a -> s+1) 0 list 
filterL p list = foldl ((\p xs x -> if p x then xs++[x] else xs) p) [] list 

lengthL_test = lengthL [3,4,8,5,6,0,4]
filterL_test = filterL (>3) [3,4,8,5,6,0,4]



isort' :: (Ord a) => [a] -> [a]
isort' list = foldr 
                (fix 
                    (\r y newL 
                        -> if newL == [] then 
                                [y] 
                            else 
                                (if y <= (head newL) then 
                                    (y : newL) 
                                else
                                    ((head newL) : (r y (tail newL)))
                                )
                        )
                    ) 
              [] list
            
-- original
isort :: (Ord a) => [ a ] -> [ a ]
isort [] = []
isort (a:xs) = ins a (isort xs)
                where
                    ins a [] = [a]
                    ins a (b:ys) | a<= b = a:(b:ys)
                                 | otherwise = b: (ins a ys)

