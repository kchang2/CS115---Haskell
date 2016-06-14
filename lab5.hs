--
-- Lab 5
-- Kai Chang
-- CS 115
--

import Control.Monad as CM

-- Part A

-- Question 1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do
             i <- [1..]
             j <- [1..i-1]
             k <- [1..j-1]
             l <- [1..k-1]
             CM.guard (i^3 + l^3 == j^3 + k^3) >> return ((i,l), (j,k), i^3 +l^3)


-- Question 2
quest2_1 :: Integer
quest2_1 = sum (do
				x <- [1..1000]
				CM.guard (x `mod` 5 == 0 || x `mod` 3 == 0) >> return x)

quest2_2 :: Integer
quest2_2 = sum (do
				x <- [1..1000]
				if x `mod` 5 == 0 || x `mod` 3 == 0
					then return x
				else mzero)

-- From before:
-- sum [x | x <- [1..1000], x `mod` 5 == 0 || x `mod` 3 == 0]
-- sum [0,3..999] + sum [0,5..1000] - sum [0,15..999]
-- 234168


-- Question 3
isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == reverse (show n)

largestPalindrome :: Integer
largestPalindrome = maximum (do
                  n <- [100..999]
                  m <- [100..999]
                  CM.guard (isPalindrome (n*m) == True) >> return (n*m))

-- Question 4
type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

exprs :: [[Item]]
exprs = do
        i1 <- ops
        i2 <- ops
        i3 <- ops
        i4 <- ops
        i5 <- ops
        i6 <- ops
        i7 <- ops
        i8 <- ops
        return [N 1, i1, N 2, i2, N 3, i3, N 4, i4, N 5, i5, N 6, i6, N 7, i7, N 8, i8, N 9]

normalize :: Expr -> Expr
normalize (e:[]) = e:[]
normalize ((N e):(O Cat):(N e1):es) = normalize (N (10*e+e1):es)
normalize (e:o:es) = e:o:(normalize (es))
normalize _ = error "This is unreal"

evaluate :: Expr -> Int
evaluate (N e:[]) = e
evaluate ((N e):(O Add):(N e1):es) = evaluate (N (e+e1):es)
evaluate ((N e):(O Sub):(N e1):es) = evaluate (N (e-e1):es)
evaluate _ = error "This is unreal"

--- Included for code needed to test our List Monad puzzle
-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs




-- Part B

-- Question 1
{-
 [1..6] >>= \n1 -> (do n1 <- [1..6]
                       []
                       return (n1, n2))

 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (do [] return (n1, n2)))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [] >> return (n1, n2))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [] >>= \ _ -> return (n1, n2))

  and because 
 	(>>=) :: [a] -> (a -> [b]) -> [b]
	mv >>= f = concatMap f mv

	concatMap :: (a -> [b]) -> [a] -> [b]
	concatMap f lst = concat (map f lst)

 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (concatMap (\_ -> return (n1, n2)) [])
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (concat (map  (\ _ -> return (n1, n2)) [])))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [])
 [1..6] >>= \n1 -> (concatMap (\n2 -> []) [1..6])
 [1..6] >>= \n1 -> (concat (map (\n2 -> []) [1..6]))
 concatMap (\n1 -> (concat (map (\n2 -> []) [1..6]))) [1..6]
 concat (map (\n1 -> (concat (map (\n2 -> []) [1..6]))) [1..6])

 	and because map (\n2 -> []) [1..6] = [[],[],[],[],[],[]]

 concat (map (\n1 -> (concat [[],[],[],[],[],[]]) ) [1..6])

 	and because map (\n1 -> (concat [[],[],[],[],[],[]]) ) [1..6] = [[],[],[],[],[],[]]

 concat ([[],[],[],[],[],[]])
 []
-}


-- Question 2
{-
 [1..6] >>= \n1 -> (do n2 <- [1..6]
                   return <anything>
                   return (n1, n2))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (do return <anything>
                                          return (n1, n2)))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> return <anything> >> return (n1, n2))                                        
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> return <anything> >>= \_ -> return (n1, n2))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (concatMap (\_ -> return (n1, n2)) (return <anything)))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (map (\_ -> return (n1, n2) (return <anything>)))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (map (\_ -> return (n1, n2)) [<anything>]))
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat [[(n1, n2)]])
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat [[(n1, n2)]])
 [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [(n1, n2)])

	which in ghci yields 
		[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]

and the other following expression 
[1..6] >>= \n1 -> (do n2 <- [1..6]
                   return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> do return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [(n1, n2)]) 

	which in ghci yields
		[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]

looking at both the desugared form and the resulting answer, we arrive at the same result.
-}


-- Question 3
{-
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]

let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
	\s' -> case s' of 
	['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
	_ -> fail "empty list" 

["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>= \s' -> case s' of 
														['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
														_ -> []

concatMap 
	(\s' -> case s' of 
		['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
		_ -> [])
	["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]

concat (map 
		(\s' -> case s' of 
			['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
			_ -> []) 
		["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])

concat [return ['x','y'], return ['z','w'], [], return ['c','c'], []]
concat [["xy"], ["zw"], [], ["cc"], []]
["xy", "zw", "cc"]

_ -> fail "Pattern match failure in do expression"
if instead we did this, we would never have to reach the concat point because the function would just return the message
	for a monad

-}

-- Question 4
{-
 Note that (k x1) is already a list -- ie. foldr, and concat both take in [[a]]
 Note k is a function
 The m = [x1,x2,...] case
 
 foldr ((++) . k) [] m
 foldr ((++) . k) [] [x1, x2, ...]
 foldr (\x r -> ((++) . k) x r) [] [x1, x2, ...]

	or equivalently,
		\x r -> (++) (k x) r
		\x -> (++) (k x)
		\x r -> kx ++ r

	this is because we note from our point operator,
		f (g x) = (f . g) x
		so ((++) . k) x r
 		(++) (k x) r

 	and note that r in this case is related to x such that
 		m = x:xs = x r

 		because from foldr's definition
 			foldr f z []     = z
			foldr f z (x:xs) = x `f` foldr f z xs
		so in this case, because we deal with a list, then
		the value r is really just the remaining parts of the
		list because lamda deals with each element.

		So x is the first argument (aka the left one when used in infix)
		and r is the second (aka the right one).
		So r ends up being "foldr f z xs"

		Furthermore, this would not necessarily be the case
		for EVERY r. This is only valid in the list case.

 ((++) . k) x1 ( foldr (\x r -> ((++) . k) x r) [] [x2, ...])
 ((++) . k) x1 ( ((++) . k) x2 ( foldr ((\x r -> (++) . k) x r) [] [...]))
 ...
 ...
 ((++) . k) x1 ( ((++) . k) x2 ( ... ( (foldr (\x r -> (++) . k x r) [] [xn]))))
 ((++) . k) x1 ( ((++) . k) x2 ( ... ( ((++) . k) xn ( (foldr (\x r -> (++) . k x r) [] [])))))

 	by foldr function definition

 ((++) . k) x1 ( ((++) . k) x2 ( ... ( ((++) . k) xn [] )))
 ((++) . k) x1 ( ((++) . k) x2 ( ... ( (k xn) ++ [] )))
 ((++) . k) x1 ( ((++) . k) x2 ( ... [kn]))) 	where kn = concatenated form of (k xn)
 ((++) . k) x1 ( ((++) . k) x2 [...])		kn blended in [...]
 ((++) . k) x1 ( [(k x2)] (++) [...])		same as (++) (k x) [...]
 ((++) . k) x1 [(k2), ...]		where k2 = concatenated form of (k x2)
 [(k x1)] ++ [k2, ...]			where k x1 = concatenated form of (k x1)
 [k1, k2, ...]

 FOR JADEN: or is it by lazy evaluation that we evaluate all the (++) until the end,
 			meaning we evaluate ((++) . k) first?? Or is that the opposite?

 concat (map k m)
 concat (map k [x1,x2,..])
 concat [(k x1), (k x2), ...]
 [k1, k2, ...] 		same as above, but wondering if you can leave as (k x1) : (k x2) : ...


 Note, I think we can also do
 	(++) = flip (foldr (:))
	concat = foldr (flip (foldr (:))) []
 but this doesn't seem as direct


 The m = [] case
 foldr ((++) . k) [] m
 foldr ((++) . k) [] []
 foldr (\x r -> ((++) . k) x r) [] []
 [] 	(from foldr's function defintiion)

 concat (map k m)
 concat (map k [])
 concat ([])
 []

 note something cool -- can do concat (k []) and still get same result
 						but main reason is because of map k [] = []
 						[] not the same as [0]
-}





-- Question 5
{- # LANGUAGE ExistentialQuantification # -}
{-data AnyNum = forall a . Num a => AnyNum a

anySum :: [AnyNum] -> AnyNum
anySum [] = AnyNum 0
anySum ((AnyNum n) : ns) =
  case anySum ns of
    AnyNum s -> AnyNum (n + s)-}

-- The problem with this code is that the n type and the ns type are not of the same kind 
-- (ie. that's why you have a type equality issue). Now, this is from the fact that n is of type a,
-- while s is of type AnyNum or of any (num) type. As Jaden hinted, when we do the add operator,
-- one cannot add two different Num types together. 

-- The solution to this problem is have the s and n be of the same (num) type so that the operator
-- can work, or have it such that the list is of the same (num) type.
-- Now I also thought about converting the (num) type to a Num and then convert that into 1 (num)
-- type for all values in that [AnyNum] list, but I don't know if that's possible because you stated
-- "There's not even a way to convert between Num types, only from Integer to Num". I thought functions
-- like toInteger took care of that? If not, then there is no way to fix it without changing the
-- AnyNum datatype.