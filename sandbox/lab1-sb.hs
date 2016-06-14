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
rangeProduct a b = a * rangeProduct a+1 b

-- question 3
prod :: Integer -> Integer -> Integer
prod a b | a > b = error "first integer greater than second integer"
prod a b = foldr (*) 1 [a..b]
-- prod = (foldr (*) 1 .) . enumFromTo

-- question 4.1
-- map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
-- map2 f [] []    = []
-- map 2 f (x:xs) (y:ys) = [0]
-- map2 f (x:xs) (y:ys)  = x f y : map2 xs f ys




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
largest x | length x == 0 = error "empty list"
largest x | length x == 1 = head x
largest (x:xs) = max (x) (largest (xs))

--
-- Part C
--

-- question 1
fib 3
--> (fib 2) + (fib 1)
--> ((fib 1) + (fib 0)) + (fib 1)
--> ((1) + (0)) + (1)
--> (1) + (1)
--> 2
-- the problem with this definition is that fact 0 = 1 should be in front of fact n = n * fact (n-1) because it should be a guard for the function. This wil run the first thing first, and never reach fact 0 = 1.

-- question 2
fact 3
--> 3 * (fact 2)
--> 3 * (2 * (fact 1))
--> 3 * (2 * (1 * (fact 0)))
--> 3 * (2 * (1 * 1))
--> 3 * (2 * 1)
--> 3 * 2
--> 6


-- question 3
reverse [1,2,3]
--> [2, 3],[1]
--> [3],[2,1]
--> [],[3,2,1]
--> [3,2,1]
-- the time complexity increases with this because as you get longer lists, your iterating over and over again in a reverse pyramidal scheme. So the longer the list is, the more iterations you will need to do, in terms of a factorial! Time is increasingly complex.


-- question 4
--> reverse [1,2,3]
--> reverse [2,3] ++ [1]
--> reverse [2]: ([3] ++ [1])
--> reverse [2]: ([3] : ([] ++ [1]))
--> reverse [2]: ([3] : ( [1] ))
--> reverse [2,3,1]
--> reverse [3,1] ++ [2]
-- the problem with this program is that it never ends! He ends up using reverse on a n-element list and then recursively runs the same function on a different (cyclic) n-element list. Theres no way it will ever get to [] = [], and this entity is entirely useless. What he can do is run it as an [a] -> [a] -> [a] and then run the guard such that [] ys = ys and fill in the ys from the (++) operator of xs and ys. The asynmptotic time complexity of this version is infinite..

-- question 5
-- Return the smallest value in a list of integers
--> head ( isort [3,1,2,5,4])
--> head ( insert 3 (isort [1,2,5,4]))
--> head ( insert 3 (insert 1 (isort [2,5,4])))
--> head ( insert 3 (insert 1 (insert 2 (isort [5,4]))))
--> head ( insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
--> head ( insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
--> head ( insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
--> head ( insert 3 (insert 1 (insert 2 (insert 5 [4]))))
--> head ( insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
--> head ( insert 3 (insert 1 (insert 2 (4 : [5])))) -- note I can keep [5] as 5 for convenience.
--> head ( insert 3 (insert 1 (2 : 4 : [5])))
--> head ( insert 3 (1 : 2 : 4 : [5]))
--> head ( 1 : insert 3 (2 : 4 : [5]))
--> head ( 1 : 2 : insert 3 (4 : [5]))
--> head ( 1: 2 : 3 : 4 : [5])
--> 1

-- question 6
-- The difference between foldr and foldl lies in the fact that fold'r' is reading from the right to left (backwards) while fold'l' is reading from the left to the right, or forwards.
--> foldr max 0 [1, 5, 3, -2, 4]
--> max 1 (foldr max 0 [5, 3, -2, 4])
--> max 1 (max 5 (foldr max 0 [3, -2, 4]))
--> max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
--> max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
--> max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
--> max 1 (max 5 (max 3 (max -2 (max 4 0))))
--> max 1 (max 5 (max 3 (max -2 4)))
--> max 1 (max 5 (max 3 4))
--> max 1 (max 5 4)
--> max 1 5
--> 5
--> foldl max 0 [1, 5, 3, -2, 4]
--> foldl max (max 0 1) [5, 3, -2, 4]
--> foldl max (max (max 0 1) 5) [3, -2, 4]
--> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
--> foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
--> foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
--> (max (max (max (max (max 0 1) 5) 3) -2) 4)
--> (max (max (max (max 1 5) 3) -2) 4)
--> (max (max (max 5 3) -2) 4)
--> (max (max 5 -2) 4)
--> (max 5 4)
--> 5
-- You will notice the difference between foldr and foldl is such that for a given function mapping /f, a head item /z, and the list /x, we can yield such:
-- foldr yields     f x1 (f x2 (f x3 (...(f xn z) ...)))
-- foldl yields     f (... (f ( f (f z x1) x2) x3) ...) xn
-- and remember that parentheses do NOT show actual order of evaluation. Since haskell is lazy, the outermost expression will be evaluated first.
