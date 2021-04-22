--Author= Bahareh Shidrang and Feliks Vdovichneko 
data B = T | F deriving Show
data Nat = Zero | S Nat deriving Show
data ZInt = Z Nat Nat deriving Show

--a
eqB :: B -> B -> B
eqB T T = T
eqB F F = T
eqB _ _ = F

nB :: B -> B
nB T = F
nB F = T

xorB :: B -> B -> B
xorB T F = T
xorB F T = T
xorB _ _ = F

orB :: B -> B -> B
orB F F = F
orB _ _ = T

andB :: B -> B -> B
andB T T = T
andB _ _ = F

iff :: B -> a -> a -> a
iff T a b = a
iff F a b = b

(=>>) :: B -> B -> B
(=>>) T F = F
(=>>) _ _ = T

eqN :: Nat -> Nat -> B
eqN Zero Zero = T
eqN _ Zero = F
eqN Zero _ = F
eqN (S a) (S b) = eqN a b 

evenN :: Nat -> B
evenN Zero = T
evenN (S Zero) = F
evenN (S (S a)) = evenN a

isDivisorN :: Nat -> Nat -> B
isDivisorN Zero _ = F
isDivisorN _ Zero = F
isDivisorN a b = isDivisorN' a b b
                    where
                        isDivisorN' Zero Zero b' = T
                        isDivisorN' Zero _ b' = F
                        isDivisorN' a Zero b' = isDivisorN' a b' b'
                        isDivisorN' (S a) (S b) b' = isDivisorN' a b b' 


halbN :: Nat -> Nat
halbN a = halbN' a Zero
        where
            halbN' Zero _ = Zero
            halbN' (S a) Zero = halbN' a (S Zero)
            halbN' (S a) b = iff (orB (eqN a b) (eqN (S a) b)) b (halbN' a (S b))

(<!) :: Nat -> Nat -> B
(<!) Zero Zero = F
(<!) Zero b = T
(<!) a Zero = F
(<!) (S a) (S b) = (<!) a b

smallN :: Nat -> Nat -> Nat
smallN a b = iff ((<!) a b) a b

ggtN :: Nat -> Nat -> Nat
ggtN Zero Zero = Zero
ggtN _ Zero = Zero
ggtN Zero _ = Zero
ggtN a b = ggtN' a b (smallN a b)
        where
            ggtN' a b Zero = Zero
            ggtN' a b (S Zero) = (S Zero)
            ggtN' a b (S c) = iff (andB (isDivisorN a (S c)) (isDivisorN b (S c))) (S c) (ggtN' a b c)


addN :: Nat -> Nat -> Nat
addN Zero Zero = Zero
addN a Zero = a
addN Zero b = b
addN a (S b) = addN (S a) b 

multN :: Nat -> Nat -> Nat
multN Zero Zero = Zero
multN _ Zero = Zero
multN Zero _ = Zero
multN a b = multN' a b Zero
            where
                multN' a Zero c = c
                multN' a (S b) Zero = multN' a b a
                multN' a (S b) c = multN' a b (addN c a) 
-------------------------------------------------------------
--b
maxSurfaces :: Nat -> Nat
maxSurfaces Zero = S Zero
maxSurfaces (S n) = addN (maxSurfaces n) (S n)

------------------------------------------------------------
--if ZInt is positive?
pZ :: ZInt -> B
pZ (Z Zero Zero) =  T
pZ (Z _ Zero) =  T
pZ (Z Zero _) =  F
pZ (Z a b) = iff ((<!) b a) T (iff (eqN a b) T F)
--if ZInt is negative?
nZ :: ZInt -> B
nZ a = nB (pZ a)

--several if-else-then...
iffC :: [(B,B)] -> B -> B
iffC [] otherwis = otherwis
iffC ((T,xZ):xs) otherwis = xZ
iffC ((F,xZ):xs) otherwis = iffC xs otherwis


--c
-- non-equal as result of integers ZInt
neqZ :: ZInt -> ZInt -> B
neqZ a b = nB (iff (orB (andB (pZ a) (pZ b)) (andB (nZ a) (nZ b))) (eqN (absZ a) (absZ b)) F)

-- bigger for both of integers ZInt
(>>>):: ZInt -> ZInt -> B
(>>>) a b = iffC [((andB (pZ a) (pZ b)),((<!) (absZ b) (absZ a))), ((andB (nZ a) (nZ b)),((<!) (absZ a) (absZ b))), ((andB (pZ a) (nZ b)),T)] F

--Nat represents a natural number. ZInt represents an integer number. In Z a b if a >= b then integer is a - b else -(b - a).
negZ :: ZInt -> ZInt
negZ (Z a1 a2) = iff ((<!) a2 a1) (Z a2 a1) (error "The input number is negative already")

multZ :: ZInt -> ZInt -> ZInt
multZ (Z a1 a2) (Z b1 b2) = Z (addN (multN a1 b1) (multN a2 b2)) (addN (multN a1 b2) (multN a2 b1))

-- the integer of ZInt is two natural number decrease from eachother Z a b= a-b
absZ :: ZInt -> Nat
absZ (Z Zero Zero) = Zero
absZ (Z a Zero) = a
absZ (Z Zero b) = b
absZ (Z (S a) (S b)) = absZ (Z a b) 

powZ :: ZInt -> Nat -> ZInt
powZ _ Zero = Z (S Zero) Zero
powZ a b = powZ' a b (Z (S Zero) Zero)
            where
                powZ' _ Zero c = c
                powZ' a (S b) c = powZ' a b (multZ c a)

isDivisorZ :: ZInt -> ZInt -> B
isDivisorZ a b = isDivisorN (absZ a) (absZ b)

-----------------------------------------------------------------
--Test functions
test_eqB :: B
test_eqB = eqB T F
test_xorB :: B
test_xorB = xorB T F
test_impli :: B
test_impli = (=>>) T F
test_eqN :: B
test_eqN = eqN (S Zero) (S Zero)
test_evenN :: B
test_evenN = evenN (S (S Zero))
test_isDivisorN :: B
test_isDivisorN = isDivisorN (S (S (S (S Zero)))) (S (S Zero))
test_halbN :: Nat
test_halbN = halbN (S (S (S (S Zero))))
test_ggtN :: Nat
test_ggtN = ggtN (S (S (S (S Zero)))) (S (S Zero))
test_maxSur :: Nat
test_maxSur = maxSurfaces (S (S Zero))
test_neqZ :: B
test_neqZ = neqZ (Z (S (S (S (S Zero)))) Zero) (Z (S (S Zero)) Zero)
test_bigger :: B
test_bigger = (>>>) (Z (S (S (S (S Zero)))) Zero) (Z (S (S Zero)) Zero)
test_negZ :: ZInt
test_negZ = negZ (Z (S (S Zero)) Zero)
test_multZ :: ZInt
test_multZ = multZ (Z (S (S (S (S Zero)))) Zero) (Z (S (S Zero)) Zero)
test_absZ :: Nat
test_absZ = absZ (Z Zero (S (S Zero)))
test_powZ :: ZInt
test_powZ = powZ (Z (S (S Zero)) Zero) (S (S Zero))
test_isDivisorZ :: B
test_isDivisorZ = isDivisorZ (Z (S (S (S (S Zero)))) Zero) (Z (S (S Zero)) Zero)

