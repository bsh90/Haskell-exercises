{-- Author: Bahareh Shidrang--}

{-- Aufgabe 1--}
{--
 -bin2dec [0,1,0,1,1,0]
 -bin2dec' 0 [0,1,0,1,1,0]
 -bin2dec' 0 [1,0,1,1,0]
 -bin2dec' 1 [0,1,1,0]
 -bin2dec' 2 [1,1,0]
 -bin2dec' 5 [1,0]
 -bin2dec' 11 [0]
 - 22
 - --}
{-- Aufgabe 2--}
--recursive
roughlyPI :: Double -> Double
roughlyPI 0 = 2
roughlyPI k = (roughlyPI (k-1)) + (roughlyPIfunc k)

--non-recursive
roughlyPIfunc :: Double -> Double
roughlyPIfunc 0 = 2
roughlyPIfunc k = (2/((2*k)+1)) * (facFunc k k)

--recursive
facFunc :: Double -> Double -> Double
facFunc k 1 = 2/(k+1)
facFunc k i = (facFunc1 k i) * (facFunc k (i-1)) 

--non-recursive
facFunc1 :: Double -> Double -> Double
facFunc1 k 1 = 2/(k+1)
facFunc1 k i = (2*i)/(k+i)


{-- Aufgabe 3--}
onlyParenthesis :: String -> String
onlyParenthesis [] = []
onlyParenthesis (x:xs)
            | x `elem` "[{()}]" = x:(onlyParenthesis xs)
            | otherwise = onlyParenthesis xs


{-- Aufgabe 4--}
hexagonalNums :: Int -> [Int]
hexagonalNums 0 = [0]
hexagonalNums n = (hexagonalNums (n-1)) ++ [(2*(n^2))-n]

{-- Aufgabe 5--}
averageInInterval ::  Float -> Float -> [Float] -> Float
averageInInterval a b [] = 0
averageInInterval a b (x:xs) = (sum l)/fromIntegral (length l)
                                where 
                                l = listInInterval a b (x:xs) 

listInInterval :: Float -> Float -> [Float] -> [Float]
listInInterval a b [] = []
listInInterval a b (x:xs)
            | (b>=a) && (x `elem` [a .. b]) = x:(listInInterval a b xs) 
            | (a>b) && (x `elem` [b .. a]) = x:(listInInterval a b xs) 
            | otherwise =  listInInterval a b xs


{-- Aufgabe 6--}
sumBin :: Int -> Int
sumBin d = sum (dec2bin d)

dec2bin :: Int -> [Int]
dec2bin d
        | d<0 = error "negative number. please input positive number."
        | d<2 = [d] 
        | mod d 2 == 0 = (dec2bin (div d 2)) ++ [0]
        | mod d 2 == 1 = (dec2bin (div d 2)) ++ [1]


{-- Aufgabe 7--}
hex2okt :: String -> String
hex2okt "" = ""
hex2okt h = (dec2oct (hex2dec h))

hexch :: Char -> Int
hexch h
        | h=='0' = 0 
        | h=='1' = 1 
        | h=='2' = 2 
        | h=='3' = 3 
        | h=='4' = 4 
        | h=='5' = 5 
        | h=='6' = 6 
        | h=='7' = 7 
        | h=='8' = 8 
        | h=='9' = 9 
        | h=='A' = 10 
        | h=='B' = 11 
        | h=='C' = 12 
        | h=='D' = 13 
        | h=='E' = 14 
        | h=='F' = 15
        | otherwise = 0 

hex2dec :: String -> Int
hex2dec [] = 0
hex2dec (x:xs) = (16^((length (x:xs))-1))*(hexch x) + (hex2dec xs) 



decch :: Int -> String
decch d
        | d==0 = "0" 
        | d==1 = "01" 
        | d==2 = "02" 
        | d==3 = "03" 
        | d==4 = "04" 
        | d==5 = "05" 
        | d==6 = "06" 
        | d==7 = "07" 
        | otherwise = "0" 


dec2oct :: Int -> String
dec2oct 0 = "0"
dec2oct d
        | d<0 = error "negative number. please input positive number."
        | d<8 = decch d
        | mod d 8 == 0 = (dec2oct (div d 8)) ++ "0"
        | mod d 8 == 1 = (dec2oct (div d 8)) ++ "1"
        | mod d 8 == 2 = (dec2oct (div d 8)) ++ "2"
        | mod d 8 == 3 = (dec2oct (div d 8)) ++ "3"
        | mod d 8 == 4 = (dec2oct (div d 8)) ++ "4"
        | mod d 8 == 5 = (dec2oct (div d 8)) ++ "5"
        | mod d 8 == 6 = (dec2oct (div d 8)) ++ "6"
        | mod d 8 == 7 = (dec2oct (div d 8)) ++ "7"
        | otherwise = "0"



{-- Test functions--}
test_roughlyPI= roughlyPI 1000
test_onlyParenthesis = onlyParenthesis "[(2+7.0)*a-(xyz), {word}]"
test_hexagonalNums = hexagonalNums 9
test_averageInInterval= averageInInterval 2 5 [2.0, 3.0, 5.0, 1.0, 0.0, 1.0]
test_sumBin = sumBin 7
test_hex2okt = hex2okt "1F81F8"

