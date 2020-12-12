{-- Bahareh Shidrang--}

{-- Aufgabe 1--}
(°) :: Integer -> Integer -> Integer
(°) k 0 = 1
(°) k n = k^((°) k (n-1)) 

{-- Aufgabe 2--}
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

{-- Aufgabe 3--}
insert ::(Eq a)=> a -> Int -> [a] -> [a]
insert new _ [] = [new]
insert new i (x:xs)
            | (i > length (x:xs)) || (i<0) = error "index is out of range of list length"
            | i == 0 = [new] ++ (x:xs)
            | otherwise = x:(insert new (i-1) xs)



{-- Aufgabe 4--}
toDecFrom :: [Int] -> Int -> Int
toDecFrom [] b = 0
toDecFrom (x:xs) b 
            | b<=0 || b>10 = error "The basis is out of [1..10]"
            | x>=b || x<0 = error "The list is out of basis limit" 
            | b==1 = length (x:xs)
            | otherwise = (b^((length (x:xs))-1))*x + toDecFrom xs b



{-- Aufgabe 5--}
removeReps ::(Eq a)=> [a] -> [a]
removeReps [] = []
removeReps (x:xs) = reverse (removereps1 (reverse (x:xs)))
                    where
                    removereps1 [] = []
                    removereps1 (a:as)
                                | a `elem` as = (removereps1 as)
                                | otherwise = a:(removereps1 as)

{-- Aufgabe 6--}
tokenizer :: [Char] -> [[Char]]
tokenizer [] = [[]]
tokenizer xs = tokenhelper xs [] [] 
            where
            tokenhelper [] [] [] = []
            tokenhelper [] [] rs = rs
            tokenhelper [] is rs = rs++[is]
            tokenhelper (x:xs) is rs
                | (x `elem` ",.? ") && (is == []) = tokenhelper xs [] rs
                | (x `elem` ",.? ") && (is /= []) = tokenhelper xs [] (rs++[is])
                | otherwise = tokenhelper xs (is++[x]) rs

{-- Aufgabe 7--}
groupEquals ::(Eq a)=> [a] -> [[a]]
groupEquals [] = [[]]
groupEquals xs = grouphelper xs [] []
                where
                grouphelper [] [] []=[]
                grouphelper [] [] rs = rs
                grouphelper [] is rs = rs++[is]
                grouphelper (x:xs) [] rs = grouphelper xs [x] rs
                grouphelper (x:xs) is rs
                    | (x `elem` is) = grouphelper xs (is++[x]) rs 
                    | otherwise = grouphelper xs [x] (rs++[is])


{-- Aufgabe 8--}
multBits :: [Int] -> [Int] -> [Int]
multBits bxs1 bxs2 = dec2bin ((toDecFrom bxs1 2)*(toDecFrom bxs2 2))
                    where
                    dec2bin :: Int -> [Int]
                    dec2bin d
                        | d<0 = error "negative number. please input positive number."
                        | d<2 = [d] 
                        | mod d 2 == 0 = (dec2bin (div d 2)) ++ [0]
                        | mod d 2 == 1 = (dec2bin (div d 2)) ++ [1]


{-- Test Functions-}
test_aufgabe1_1 = (°) 3 3
test_aufgabe1_2 = 2°4
test_aufgabe1_3 = 4 ° 3

test_flatten = flatten [[8,2], [3], [], [4,5,0,1], [1]]

test_insert = insert '3' 3 "Hello"

test_toDecFrom = toDecFrom [1, 3, 2, 1] 4

test_removeReps_1 = removeReps [1, 1, 2, 0, 2, 1, 3, 1, 4]
test_removeReps_2 = putStr (removeReps "Freie Universität Berlin" ++ "\n")
test_removeReps_3 = removeReps "Freie Universitat Berlin"

test_tokenizer = tokenizer "?We are bahar,manuel."
test_group = groupEquals [1,1,2,2,1,2,2,1,1,1]
test_multBits = multBits [1,0,0,1] [1,0,1,0]

