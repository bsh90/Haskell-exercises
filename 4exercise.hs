{-- Author: Bahareh Shidrang --}
import Data.List

{-- Aufgabe 1--}
isSorted cmp xs = and (zipWith cmp xs (tail xs))


bubbleSort :: (Ord a)=> [a] -> [a]
bubbleSort xs | isSorted (<=) xs = xs 
              | otherwise = bubbleSort (moveBubble xs)
                where
                moveBubble [] = []
                moveBubble [x] = [x]
                moveBubble (x:y:rest) | (<=) x y = x: moveBubble (y:rest)
                                      | otherwise = y: moveBubble (x:rest)


traceBubbleSort :: (Ord a) => [a] -> [[a]]
traceBubbleSort xs = reverse ((takeWhile (not.isSorted (<=)) (iterate moveBubble xs)) ++ [bubbleSort xs])
                where
                moveBubble [] = []
                moveBubble [x] = [x]
                moveBubble (x:y:rest) | (<=) x y = x: moveBubble (y:rest)
                                      | otherwise = y: moveBubble (x:rest)

{-- Aufgabe 2.a--}
p :: Integer -> [Integer]
p n = sieb [2..n]
            where
            sieb [] = []
            sieb (p1:xs) = p1: sieb [k | k<-xs, (mod k p1)>0]


weakGoldbachTriples :: Integer -> [(Integer, Integer, Integer)]
weakGoldbachTriples n | (n<=5) || (mod n 2 == 0) = error "Enter an odd number bigger than 5"
                      | otherwise = cartesianProd (p n) (p n) (p n) n
                        where
                        cartesianProd  xs ys zs n= [(x,y,z) | x <- xs, y <- ys, z <- zs, ((x+y+z)==n), (x<=y && y<=z)]
{--
weakGoldbachTriples :: Integer -> [(Integer, Integer, Integer)]
weakGoldbachTriples n | (n<=5) || (mod n 2 == 0) = error "Enter an odd number bigger than 5"
                      | otherwise = bachhelper (p n) (p n) (p n) (p n) []
                        where
                        bachhelper [] [] [] sl ss= []
                        bachhelper (a:ax) [] [] sl ss= bachhelper ax sl sl sl ss
                        bachhelper (a:ax) [] (c:cx) sl ss= bachhelper ax sl (c:cx) sl ss
                        bachhelper (a:ax) (b:bx) [] sl ss= bachhelper (a:ax) bx sl sl ss
                        bachhelper (a:ax) (b:bx) (c:cx) sl ss
                                | ((a + b + c) == n) = (a, b, c):ss 
                                | otherwise= bachhelper (a:ax) (b:bx) cx sl ss

--}

{-- Aufgabe 2.b--}
wGTriplesUntil :: Integer -> Bool
wGTriplesUntil n | (n<= 5) = error "Enter number bigger than 5"
                 | otherwise = and (map iswG [7,9..n]) 
                    where
                    iswG n | sum' (head (weakGoldbachTriples n)) == n = True
                           | otherwise = False
                            where
                            sum' (a, b, c) = a+b+c

{-- Aufgabe 3.a--}
diffList :: (Eq a)=> [a] -> [a] -> [a]
diffList [] [] = [] 
diffList xs1 xs2 = filter (`notElem` xs2) xs1
{-- Aufgabe 3.b--}
firstNatNotIn :: [Int] -> Int
firstNatNotIn [] = 0
firstNatNotIn xs = head (diffList [0..(maximum xs)] xs)

{-- Aufgabe 4-}
toDecFrom :: [Int] -> Int -> Int
toDecFrom xs b | (b<=1 || b>=10) = error "Please enter a basis between 2 until 9"
               | otherwise = foldl ((+).(*b)) 0 xs
--               | otherwise = (sum (zipWith (*) xs (map (b^) [((length xs)-1), ((length xs)-2)..0])))

{-- foldl f k [x1,x2,x3,x4] = f(f(f(f k x1)x2)x3)x4
 - toDecFrom [a,b,c] B = 
 -   a*B^2 + b*B^1 + c
 -  (a*B + b)*B + c
 - ((0*B + a)*B + b)*B + c
 - --}

{--Aufgabe 5 a,b,c--}
flatten_a ::(Eq a)=> [[a]] -> [a]
flatten_a xs = foldr (++) [] xs

flatten_b ::(Eq a)=> [[a]] -> [a]
flatten_b xs = foldl (++) [] xs

{-- Part C: To produce result, foldl has to go through all the element of list, especially in large lists it makes
 - problem such as stack overflow
 - foldr if we operate it with lazy in first argument, it can produce results quickly
 - In case in first argument, we use strict and the consuming function of these fold are strict, we can use foldl',
 - which is more efficient than foldl.--}

{-- Aufgabe 6--}
addMatrix ::(Num a)=> [[a]] -> [[a]] -> [[a]] 
addMatrix as bs | (length as == length bs) && ((map length as) == (map length bs)) = zipWith (zipWith (+)) as bs
                | otherwise = error "Please insert the equal matrices"

multiMatrix ::(Num a)=> [[a]] -> [[a]] -> [[a]]
multiMatrix as bs | (length (nub (map length as)) ==1) && ((head (map length as)) == length bs) = [[sum (zipWith (*) a b) | b <- (flipMat bs)] | a <- as]
                  | otherwise = error "Please insert n*m and m*k matrices"
                    where
                    flipMat bs = [map (!! i) bs | i <- [0..((length (head bs))-1)]]

{-- Aufgabe 7--}
unfold :: (a-> Bool) -> (a->b) -> (a->a) -> a -> [b]
unfold p f g x 
    | p x = []
    | otherwise = (f x):(unfold p f g (g x))


map1 :: (a->b) -> [a] -> [b]
map1 f xs= unfold null (f.head) tail xs

iterate1 :: (a->a) -> a -> [a]
iterate1 f x= unfold (\_ -> False) (\a -> a) f x
                
dec2bin1 :: Int -> [Int]
dec2bin1 d  | d<2 = [d]
            | otherwise = reverse (unfold (\d -> (d==0)) (`mod` 2) (`div` 2) d)
  
{-- Test Functions--}
test_traceBubble = traceBubbleSort [0,1,3,8,0]
test_bach = weakGoldbachTriples 19
test_bachUntil = wGTriplesUntil 300
test_diffList = diffList "Sebastian Meyer" "aaaaennn"
test_firstNatNotIn = firstNatNotIn [3,2,0,1,8,9,12,6]
test_toDecFrom = toDecFrom [1,3,2] 4 
test_flatten_a = flatten_a [[8,2], [3], [], [4,5,0,1], [1]]
test_flatten_b = flatten_b [[8,2], [3], [], [4,5,0,1], [1]]
test_addMatrix = addMatrix [[1,2],[3,4],[5,6]] [[0,1],[2,5],[3,5]]
test_multiMatrix = multiMatrix [[1,2],[3,4],[5,6]] [[10,20,30,40],[60,70,80,90]]
test_map1 = map1 (+1) [1,2,3,4]
test_iterate1 = iterate1 (+1) 1
test_dec2bin1 = dec2bin1 42
