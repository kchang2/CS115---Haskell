--
-- CS 115
-- Lab 3
-- Kai Chang
--

module Lab3ab where


--
-- Part A
--

-- question 1
-- class Eq a where
-- (==) :: a -> a -> Bool
-- (/=) :: a -> a -> Bool
--
--data Nat = Zero | Succ Nat
{-instance Eq Nat where
         (==) Zero Zero = True
         (==) Zero _ = False
         (==) _ Zero = False
         (==) (Succ n1) (Succ n2) = n1 == n2

instance Show Nat where
         show Zero = "Zero"
         show Succ Zero = "Succ Zero"
         show (Succ n) = "Succ (" ++ show n ++ ")"-}


-- question 2
data Nat = Zero | Succ Nat
     deriving (Eq, Show)


-- question 3
instance Ord Nat where
--         compare (Succ x) (Succ y) | x == y = EQ
--                                   | x < y = LT
--                                   | otherwise = GT
         (<=) Zero _ = True
         (<=) _ Zero = False
         (<=) (Succ x) (Succ y) = x <= y
--         max (Succ x) (Succ y) = max x y
--         min (Succ x) (Succ y) = not (max x y)

-- Haskell can derive Ord instance for us for Nat because in the Nat
-- datatype, Zero is a smaller constructor than Succ Nat.
         
-- question 4
data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)

-- In this case, Haskell CANNOT derive Ord because although in most cases Neg Nat
-- is less than Pos Nat, this condition is invalid for Neg Zero and Pos Zero.

instance Eq SignedNat where
         (==) (Pos Zero) (Neg Zero) = True
         (==) (Neg Zero) (Pos Zero) = True
         (==) (Pos _ ) (Neg _ ) = False
         (==) (Neg _ ) (Pos _ ) = False
         (==) (Pos n1) (Pos n2) = n1 == n2
         (==) (Neg n1) (Neg n2) = n1 == n2

instance Ord SignedNat where
         (<=) (Pos Zero ) (Neg Zero ) = True
         (<=) (Pos _ ) (Neg _ ) = False
         (<=) (Neg _ ) (Pos _ ) = True
         (<=) (Pos n1 ) (Pos n2 ) = n1 <= n2
         (<=) (Neg n1 ) (Neg n2 ) = n1 >= n2

-- Question 5
addNat :: Nat -> Nat -> Nat
addNat Zero n2 = n2
addNat (Succ n1) n2 = Succ (addNat n1 n2)

subNat :: Nat -> Nat -> Nat
subNat n1 n2 | n1 < n2 = error "This yields a negative value, which is not part of the Nat datatype."
subNat Zero (Succ _) = error "This operation yields a negative value, or a non-Nat datatype."
subNat n1 Zero = n1
subNat (Succ n1) (Succ n2) = subNat n1 n2

mulNat :: Nat -> Nat -> Nat
mulNat _ Zero = Zero
mulNat n1 (Succ n2) = addNat n1 (mulNat n1 n2)

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos n1) (Pos n2) = Pos (addNat n1 n2)
addSignedNat (Neg n1) (Neg n2) = Neg (addNat n1 n2)
addSignedNat (Neg n1) (Pos n2) | n1 > n2 = Neg (subNat n1 n2)
                               | otherwise = Pos (subNat n2 n1)
addSignedNat x y = addSignedNat y x

mulsignedNat :: SignedNat -> SignedNat -> SignedNat
mulsignedNat (Pos n1) (Neg n2) = Neg (mulNat n1 n2)
mulsignedNat (Neg n1) (Pos n2) = Neg (mulNat n1 n2)
mulsignedNat (Pos x) (Pos y) = Pos ( mulNat x y)
mulsignedNat (Neg x) (Neg y) = Pos ( mulNat x y)

sig :: SignedNat -> SignedNat
sig (Neg Zero) = Neg Zero
sig (Pos Zero) = Pos Zero
sig (Neg _) = (Neg (Succ (Zero)))
sig (Pos _) = (Pos (Succ (Zero)))

fknegate :: SignedNat -> SignedNat
fknegate (Neg n) = Pos n
fknegate (Pos n) = Neg n

fkabs :: SignedNat -> SignedNat
fkabs (Neg n) = Pos n
fkabs (Pos n) = Pos n

intToNat :: Integer -> SignedNat
intToNat n | n >= 0 = Pos (natu n)
           | otherwise = Neg (natu (negate n))
           where natu :: Integer -> Nat
                 natu 0 = Zero
                 natu n = Succ (natu (n-1))

instance Num SignedNat where
       (+) n1 n2 = addSignedNat n1 n2
       (-) n1 n2 = addSignedNat n1 (negate n2)
       (*) n1 n2 = mulsignedNat n1 n2
       negate n = fknegate n
       abs n = fkabs n
       signum n = sig n
       fromInteger n = intToNat n


-- Question 6
intsum :: Nat -> Integer
intsum Zero = 0
intsum (Succ n) = 1 + (intsum n)

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos n) = intsum n
signedNatToInteger (Neg n) = negate (intsum n)


-- Question 7
-- The problem with signedNat I think is the fact that it is redundant in all the checks to see if the values are positive, negative, or one of each.
-- That zero sucks though, because of the redundancy in Neg Zero and Pos Zero
-- data UnaryInteger = Zero | Neg Nat | Pos Nat -- unary function + Num(ber)
-- A way to fix this could be to have a new datatype starting at 1 such taht we get One | Succ _
-- and then when we use UnaryInteger, we can have it such that
-- UnaryInteger = Zero | Neg _ | Pos _, which now eliminates the repetitive zero case.

-- Question 8
factorial :: (Num a, Ord a) => a -> a
-- applied to an argument of any time supporting numerical and comparison operations, and return value of the same type
factorial n | n < 0 = error ("Value is negative")
factorial 0 = 1
factorial n = n * factorial (n-1)
-- Results of test:
-- *Lab3ab> factorial (Pos (Succ (Succ (Succ Zero))))
-- Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))


--
-- Part B
--

-- Question 1.1
-- The operator >#< comparing the two scores is infix or non-associative because of the fact that it is explicitly taking in an Integer, so the fact that we do 5 >#< 5 >#< 4, in either way will yield type errors.

-- Question 1.2
-- The operator +| works for both cases (infixr and infixl) and we will declare it infixl. This is because adding is associative (mathematically), and because we are simply dealing with the unit terms, which is deterministic value (not affected by carrying over digits), this will stay the same regardless of all operations (ie. modulus).
-- See 7 + 6 + 5 ==> infixr 7+1 = 8, infixl 3+5 = 8

-- Question 1.3
-- The operator is infixl only, as you consider the fact we have a &< [a] &< a and [a] &< a &< a, you must return a list and have the next (&<) be of [a] a format, and only infixl will allow such results. a &< a &< [a] is not considered because it does not match the initial example (condition) of [1, 2] &< 3 = [1, 2, 3]. Note that the element appends to the end of the list.

-- Question 1.4
-- The operator is infixr only because like the previous problem, the format is such that the function takes in a >&& [a], and uses the previous method but in reverse order. Note this is appending to the beginning, different from the previous problem in that sense.


-- Question 2
-- Consider +# --> 3 +# 12 +# 100
-- (3 +# 12) +# 100 --> 2 +# 100 --> 3
-- 3 +# (12 +# 100) --> 2 +# 3 --> 1
-- 10 +# 100 +# 1000
-- (10 +# 100) +# 1000 --> 3 +# 1000 --> 4
-- 10 +# (100 +# 1000) --> 10 +# 4 --> 2
-- (99 +# 100) +# 1000 --> 3 +# 1000 --> 4
-- 99 +# (100 +# 1000) --> 99 +# 4 --> 3
-- (1 +# 100) +# 9999 --> 3 +# 9999 --> 5
-- 1 +# (100 +# 9999) --> 1 +# 5 --> 1
-- So you will see that if it is infixl, we will be dependent on the highest order value (ie. the most digits), while infixr will be dependent on the lowest order value (ie. the smallest digit size).
-- It's associativity could be both infixr or infixl, but it should be infix. Has no real concrete meaning as some values used above for the operation resulted in different answers for infixl and infixr. Thus it makes most sense to have this operation as infix.
