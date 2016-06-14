--
-- Kai Chang
-- CS 115
-- Lab 1
--


--
-- Part A
--

-- question 1a
(+*) :: Double -> Double -> Double
(+*) x y = x*x + y*y -- can also write it as x +* y = x*x + y*y
infixl 7 +*

-- question 1b
(^||) :: Bool -> Bool -> Bool
(^||) bool1 bool2 = (bool1 /= bool2)
-- (^||) b1 b2 | b1 = b2 = False
-- 		| otherwise = True
infixr 3 ^||

-- question 2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct a b | a > b = error "first integer greater than second integer"
                 | a == b = b
rangeProduct a b = a * rangeProduct (a+1) b

-- question 3
prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2 :: Integer -> Integer -> Integer 
rangeProduct2 a b | a > b = error "first integer greater than second integer"
rangeProduct2 a b = prod [a..b]

-- question 4.1
-- I choose a, b, c because they could be of different types
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] [] = [] -- if both list returns empty
map2 _ _ [] = [] -- if one list is empty
map2 _ [] _ = []
map2 f (x:xs) (y:ys)  = f x y : map2 f xs ys -- recursive mapping

-- question 4.2
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] [] [] = [] -- if empty lists for all 3
map3 _ _ [] [] = [] -- if any list is empty
map3 _ [] _ [] = []
map3 _ [] [] _ = []
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : map3 f xs ys zs

-- question 4.3
-- dot :: [Integer] -> [Integer] -> Integer
-- dot = (sum .) . map2 (*)
-- > dot = \lst1 -> (sum .) . map2 (*) lst1
-- > dot = \lst1 -> (sum .) (map2 (*) lst1)
-- > dot = \lst1 -> sum . map2 (*) lst1
-- > dot lst1 = sum . map2 (*) lst1
-- > dot lst1 = \lst2 -> sum . map2 (*) lst1 lst2
-- > dot lst1 = \lst2 -> sum (map2 (*) lst1 lst2)
-- > dot lst1 lst2 = sum (map2 (*) lst1 lst2)

-- question 5
-- sum [x | x <- [1..1000], x `mod` 5 == 0 || x `mod` 3 == 0]
-- sum [0,3..999] + sum [0,5..1000] - sum [0,15..999]
-- 234168

-- question 6
primes = sieve [2..]
    where
        sieve :: [Integer] -> [Integer]
        sieve [] = []
        sieve (x:xs) = x : sieve (filter (\n -> n `mod` x /= 0) xs) -- removes all values in list that are multiples of x
-- sum (takeWhile (< 10000) primes)
-- 5736397

--
-- Part B
--

-- question 1
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (lst:lsts) = lst + sumList(lsts)

-- question 2
-- The problem with this recursive definition is that it's takes more time than needed because it takes in the entire list over and over again, rather than looking at one element and comparing it to the remaining of the list.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest (x:[]) = x
largest (x:xs) = max x (largest xs)

--
-- Part C
--

-- question 1
-- fib 3
-- > fib (3-1) + fib (3-2)
-- > fib (2) + fib (3-2)
-- > (fib (2-1) + fib (2-2)) + fib (3-2)
-- > (fib (1) + fib (2-2)) + fib (3-2)
-- > (1 + fib (2-2)) + fib (3-2)
-- > (1 + fib(0)) + fib (3-2)
-- > (1 + 0) + fib (3-2)
-- > 1 + fib (3-2)
-- > 1 + fib (1)
-- > 1 + 1
-- > 2
-- note that in lazy evaluation, they would actually recognize fib 1 twice and calculate that together at the same time. For the intensive purpose of this assignment, it is kept left to right.

fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1
-- question 2
-- fact 3
-- > 3 * fact (3-1)
-- > 3 * ((3-1) * fact ((3-1)-1))
-- > 3 * (2 * (fact ((3-1)-1)))
-- > 3 * (2 * (((3-1)-1) * fact (((3-1)-1)-1)))
-- > 3 * (2 * ((2-1) * fact (((3-1)-1)-1)))
-- > 3 * (2 * (1 * fact (((3-1)-1)-1)))
-- > ..
-- > ..
-- > 3 * (2 * (1 * (0 * (.. * (((-n+1)-1) fact (((((((3-1)-1)-1)-1)-)..)-n)))))..)
-- do not need to convert 3-1 to 2 because we don't pattern match for anything (so we do not need to check explicit value). (-) operation is still from Integer->Integer->Integer so it is good.
-- the problem with this definition is that fact 0 = 1 should be in front of fact n = n * fact (n-1) because it should be a guard for the function. This wil run the first thing first, and never reach fact 0 = 1.

-- question 3
-- reverse [1,2,3]
-- > iter [1,2,3] []
-- > iter [2,3] (1:[])
-- > iter [3] (2:1:[])
-- > iter [] (3:2:1:[])
-- > 3:2:1:[]
-- > [3,2,1]
-- this time complexity for this function with a big n-item list is O(n) time because with an increase of an element in a list, it runs iter once more because of placing the item at the end of the first list.


-- question 4
-- reverse [1,2,3]
-- > reverse [2,3] ++ [1]
-- > (reverse [3] ++ [2]) ++ [1]
-- > ((reverse [] ++ [3]) ++ [2]) ++ [1]
-- > (([] ++ [3]) ++ [2]) ++ [1]
-- > ([3] ++ [2]) ++ [1] <--- remember that (++) is right associative, but we don't really give a fish because paranthesis "forces it" left associative!
-- > (3 : ([] ++ [2])) ++ [1]
-- > 3 : (([] ++ [2])) ++ [1] )
-- > 3 : ([2] ++ [1])
-- > 3 : 2 : ([] ++ [1])
-- > 3 : 2 : [1]
-- > [3,2,1]
-- the problem with his code is that he believes the that his reverse function is total time linear or O(n) time. This is is the case for the first part, which he got correct. However, his assumption that the (++) list of operators is O(1) is incorrect, as we can see it is also O(n) time, making the function reverse as a WHOLE O(n^2) time. ++ Is NOT left associative, and even it it was, it woudl still calculate in O(n) time from the fact that it has recursive methods and pattern matching.

-- question 5
-- Return the smallest value in a list of integers .. not sure if this is correct (doesn't seem likely)
-- head ( isort [3,1,2,5,4])
-- > head ( insert 3 (isort [1,2,5,4]))
-- > head ( insert 3 (insert 1 (isort [2,5,4])))
-- > head ( insert 3 (insert 1 (insert 2 (isort [5,4]))))
-- > head ( insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
-- > head ( insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
-- > head ( insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
-- > head ( insert 3 (insert 1 (insert 2 (insert 5 [4]))))
-- > head ( insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
-- > head ( insert 3 (insert 1 (2 : 4 : insert 5 [])))
-- > head ( insert 3 (1 : 2 : 4 : insert 5 []))
-- > head ( 1: insert 3 (2 : 4 : insert 5 []))
-- > 1

-- question 6
-- The difference between foldr and foldl lies in the fact that fold'r' is reading from the right to left (backwards) while fold'l' is reading from the left to the right, or forwards.
-- foldr max 0 [1, 5, 3, -2, 4]
-- > max 1 (foldr max 0 [5, 3, -2, 4])
-- > max 1 (max 5 (foldr max 0 [3, -2, 4]))
-- > max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
-- > max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
-- > max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
-- > max 1 (max 5 (max 3 (max -2 (max 4 0))))
-- > max 1 (max 5 (max 3 (max -2 4)))
-- > max 1 (max 5 (max 3 4))
-- > max 1 (max 5 4)
-- > max 1 5
-- > 5
-- foldl max 0 [1, 5, 3, -2, 4]
-- > foldl max (max 0 1) [5, 3, -2, 4]
-- > foldl max (max (max 0 1) 5) [3, -2, 4]
-- > foldl max (max (max (max 0 1) 5) 3) [-2, 4]
-- > foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
-- > foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
-- > (max (max (max (max (max 0 1) 5) 3) -2) 4)
-- > (max (max (max (max 1 5) 3) -2) 4)
-- > (max (max (max 5 3) -2) 4)
-- > (max (max 5 -2) 4)
-- > (max 5 4)
-- > 5
-- You will notice the difference between foldr and foldl is such that for a given function mapping /f, a head item /z, and the list /x, we can yield such:
-- foldr yields     f x1 (f x2 (f x3 (...(f xn z) ...)))
-- foldl yields     f (... (f ( f (f z x1) x2) x3) ...) xn
-- The space complexity of both foldl and foldr have O(n) space complexity, as with every increasing element, there is one additional max function to be added. However, the lazy evaluation of Haskell actually hurts foldl (versus strict evaluation on foldl) because Haskell would need run through all the recursions to get to the final state of +'s before it begins to evaluate anything. If it was strict evaluation, foldl would then evaluate at a constant space (evaluating the max function of comparison values each time before preceding on to max of elements and list). foldr does not have this issue because it is tail recursive, meaning that it will need to reach the end before it can even evaluate the max of single element values for both strict and lazy.
