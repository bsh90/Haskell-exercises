{- Author: Bahareh Shidrang-}

--Aufgabe 1
test_iterate1 = iterate (*(-1)) 1
test_iterate2 = iterate ((+1).(*2)) 0
test_iterate3 = iterate (\(a,b) -> (b, (a+b))) (0,1)

--Aufgabe 2 
mult :: Integer -> Integer -> Integer
mult n 0 = 0
mult n m = mult n (m-1) + n
-- Since we have one recursive and plus after that, it belongs to O(n)
russMult :: Integer -> Integer -> Integer
russMult n 0 = 0
russMult n m | (mod m 2)==0 = russMult (n+n) (div m 2)
             | otherwise = russMult (n+n) (div m 2) + n
-- we have even and odd recursive so it's O(n)

-- Aufgabe 3
classifyRhymeWords :: [Char] -> [[[Char]]]
classifyRhymeWords xs = rythem [] [] [] [] (wordsOut [] [] xs)
                        where
                        rythem [] rlist collector ryth [] = (collector:rlist)
                        rythem [] rlist [] [] (x1:xs) = rythem [] rlist [x1] (reverse (take 3 (reverse x1))) xs 
                        rythem rest rlist collector ryth [] = rythem [] (collector:rlist) [] [] rest
                        rythem rest rlist collector ryth (x2:xs) 
                            | ((reverse (take 3 (reverse x2))) == ryth) = rythem rest rlist (x2:collector) ryth xs
                            | otherwise = rythem (x2:rest) rlist collector ryth xs
                        wordsOut wlist collector [] = wlist
                        wordsOut wlist collector (x:xs) 
                            | (x `elem` " .,:?!") && (collector /= []) =  wordsOut (collector:wlist) [] xs
                            | (x `elem` " .,:?!") && (collector == []) = wordsOut wlist [] xs
                            | otherwise = wordsOut wlist (collector ++ [x]) xs
 
--Aufgabe 4
selectionSort :: (Num t1, Num t2, Ord a) => (t1 -> t2 -> Bool) -> [a] -> [a]
selectionSort f [] = []
selectionSort f xs = (calculateFirst f xs):(selectionSort f (deleteElem (calculateFirst f xs) xs))
    where
    calculateFirst f xs | (f 2 1) = maximum xs
                        | otherwise = minimum xs  
    deleteElem a xs = (takeWhile (\b->b/=a) xs)++(tail (dropWhile (\b->b/=a) xs))
{-calculateFirst goes over elements one time, it belogns to O(n)
deleteElem goes over elements oen time, it belogns to O(n)
selectionSort has one recursive and each has O(n), so it belongs to O(n^2)
-}

--Aufgabe 5
pyth_tripels :: (Num a, Ord a) => a -> [(a, a, a)]
pyth_tripels n = pyth_help [] n 1 1 1
    where
        pyth_help pyL m a b c 
            | ((a > m) ||(b > m) ||(c > m)) = error "error, a b c shouldn't be bigger"
            | a == m = pyL
            | ((a^2)+(b^2)==(c^2)) =  pyth_help (pyL++[(a,b,c)]) m (a+1) (a+1) (a+1)
            | b == m = pyth_help pyL m (a+1) (a+1) (a+1)
            | c == m = pyth_help pyL m a (b+1) (b+1)
            | otherwise = pyth_help pyL m a b (c+1)
-- this function goes over the n about n times, it belongs to O(n)


--Aufgabe 6
data Color = RGB (Integer,Integer,Integer) | CMYK (Double,Double,Double,Double)
            deriving Show
            
rgb2cmyk :: Color -> Color
rgb2cmyk (RGB (0,0,0)) = CMYK (0,0,0,1)
rgb2cmyk (RGB (r,g,b)) = CMYK ((1-((fromIntegral r)/(255*w))),(1-((fromIntegral g)/(255*w))),(1-((fromIntegral b)/(255*w))),(1-w))
            where 
                w = maximum [(fromIntegral r)/255, (fromIntegral g)/255, (fromIntegral b)/255]


--Aufgabe 7
type RootNum = (Integer,Integer)
rootNumAdd :: RootNum->RootNum->RootNum
rootNumAdd (a,b) (c,d) = ((a+c),(b+d))

rootNumSub :: RootNum->RootNum->RootNum
rootNumSub (a,b) (c,d) = ((a-c),(b-d))

rootNumMult :: RootNum->RootNum->RootNum
rootNumMult (a,b) (c,d) = (((a*c) + (c*d*2)),((a*d) + (b*c)))

getValue :: RootNum->Double
getValue (a,b) = (fromIntegral a)+((fromIntegral b)*(sqrt 2))

--Aufgabe 8
majority :: (Eq a) => [a] -> Maybe a
majority xs = majority_helper xs xs
            where
            majority_helper xs [] = Nothing
            majority_helper xs (x1:xs1) | (fromIntegral (length (filter (\a->a==x1) xs))) >= (((fromIntegral (length xs))/2.0)+1) = Just x1
                                        | otherwise = majority_helper xs xs1



--Rest of test functions

test3 = classifyRhymeWords "Nikolaus baut ein Haus aus Holz und klaut dabei ein Bauhaus."
test41 = selectionSort (<) [2,1,5,0,4,3,7]
test42 = selectionSort (>) [2,1,5,0,4,3,7]

test5 = pyth_tripels 16
test61 = rgb2cmyk (RGB (0,0,0))
test62 = rgb2cmyk (RGB (0,0,255))

