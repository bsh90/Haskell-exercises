{-- Exercise 1. Author: Bahareh Shidrang --}


{-- Aufgabe 1 --} 
-- If the n is negetive, it will give -1, so we should consider the absolut value of it. 
ungerade :: Integer -> Bool
ungerade n = abs(rem n 2) == 1

{-- Aufgabe 2 --}
type Center = (Double, Double)
type Radius = Double
type Circle = (Center, Radius)

area :: Circle -> Double
area c = 3.14*((snd c)**2)

perimeter :: Circle -> Double
perimeter c = 2*3.14*(snd c)

equal :: Circle -> Circle -> Bool
equal c1 c2 = (fst c1 == fst c2) && (snd c1 == snd c2)

--distance between the center of two circles would be less than sum of radius
intersect :: Circle -> Circle -> Bool
intersect c1 c2 = ((fst (fst c1)- fst (fst c2))**2+(snd (fst c1)- snd (fst c2))**2)**0.5 <= (abs(snd c1) + abs(snd c2)) 

contain :: Circle -> Circle -> Bool
contain c1 c2 = ((fst (fst c1)- fst (fst c2))**2+(snd (fst c1)- snd (fst c2))**2)**0.5 <= (abs(snd c1) - abs(snd c2)) 


{-- Aufgabe 3--}
paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                  where
                  paint 0  []     = []
                  paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                  paint n (c:cs)  = c: (paint (n-1) cs)

{-- Aufgabe 3: Funktions --}
diag :: (Int,Int,Int) -> Char
diag (x,y,size) 
    |((rem x 10) == (rem y 10)) = ' ' 
    | otherwise ='0'

quad :: (Int,Int,Int) -> Char
quad (x,y,size) 
    | (x>s && x<3*s && ((rem y 10 >5) || (rem y 10 ==0))) = ' ' 
    | otherwise ='+'
    where
    s = div size 4

diamon :: (Int,Int,Int) -> Char
diamon (x,y,size) 
    | ((x+y)>h && (x+y)<3*h && abs(x-y)<h) = '0' 
    | otherwise =' '
    where
    h = div size 2

circle :: (Int,Int,Int) -> Char
circle (x,y,size)
    | contain ((hD, hD), (hD-(2+qD))) ((xD, yD), 0)= ' '
    | contain ((hD, hD), hD-2) ((xD, yD), 0)= '-'
    | otherwise ='0'
    where
    h = div size 2
    q = div h 4
    qD = (fromIntegral q) +0.0001
    hD = (fromIntegral h) +0.0001
    xD = (fromIntegral x) +0.0001
    yD = (fromIntegral y) +0.0001


flag :: (Int,Int,Int) -> Char
flag (x,y,size)
    | (topleft (x,y,size)) && not (flagCenter (x,y,size)) = topleftsymbole (x,y,size)
    | (topright (x,y,size)) && not (flagCenter (x,y,size)) = toprightsymbole (x,y,size)
    | (bottom (x,y,size)) && not (flagCenter (x,y,size)) = bottomsymbole (x,y,size)
    | otherwise = ' '


topleft :: (Int,Int,Int) -> Bool
topleft (x,y,size) = x<=(div size 2) && y<(div size 2)
topright :: (Int,Int,Int) -> Bool
topright (x,y,size) = x>(div size 2) && y<(div size 2)
bottom :: (Int,Int,Int) -> Bool
bottom (x,y,size) = y>=(div size 2)
flagCenter :: (Int,Int,Int) -> Bool
flagCenter (x,y,size) = x>(div size 4) && x<(3*(div size 4)) && y>(div size 4) && y<(3*(div size 4))
topleftsymbole :: (Int,Int,Int) -> Char
topleftsymbole (x,y,size) 
    | (rem x 4 == 0) || (rem x 4 == 1) = '|'
    | otherwise = ' '
toprightsymbole :: (Int,Int,Int) -> Char
toprightsymbole (x,y,size)
    | (rem y 4 == 0) || (rem y 4 == 1) = '*'
    | otherwise = ' '
bottomsymbole :: (Int,Int,Int) -> Char
bottomsymbole (x,y,size)
    | (rem x 4 == 0) || (rem x 4 == 1) = '+'
    | otherwise = ' '



{--Aufgabe 4 --}
num2GermanWords :: Int -> String 
num2GermanWords n = 
    case (n) of
    0 -> "Null"
    1 -> "Eins"
    2 -> "Zwei"
    3 -> "Drei"
    4 -> "Vier"
    5 -> "Funf"
    6 -> "Sechs"
    7 -> "Sieben"
    8 -> "Acht"
    9 -> "Neun"
    10 -> "zehn"
    11 -> "elf"
    12 -> "zwolf"
    16 -> "sechzehn"
    17 -> "siebzehn"
    20 -> "zwanzig"
    60 -> "sechzig"
    70 -> "siebzig"
    otherwise -> num2deu n
    

num2deu :: Int -> String
num2deu n
    | (n>=13 && n<=19) = num2GermanWords (rem n 10) ++ num2GermanWords 10
    | (n>=30 && n<=90 && rem n 10 ==0) = num2GermanWords (div n 10) ++ "zig"
    | (n>=30 && n<=90 && rem n 10 ==1) = "Einund" ++ num2GermanWords (10*(div n 10))
    | (n>20 && n<100 && rem n 10 /=0 && rem n 10 /=1) = num2GermanWords (rem n 10) ++ "und" ++ num2GermanWords (10*(div n 10))
    | (n<0 && n<100) = "minus " ++ num2GermanWords (abs(n))
    | otherwise = "More than two digits number" 




{- Testfunktionen -}
test_inter = intersect ((3.0, 3.0), 2.5) ((9.5, 8.0), 2.0)
test_contain = contain ((1.0, 1.0), 6.0) ((2.0, 1.0), 3.0)

test_diag = paintChars diag 40
test_quad = paintChars quad 40
test_diamon = paintChars diamon 40
test_flag = paintChars flag 40
test_circle = paintChars circle 40

test_num2Ger1 = num2GermanWords 0
test_num2Ger2 = num2GermanWords 21
test_num2Ger3 = num2GermanWords (-21)

