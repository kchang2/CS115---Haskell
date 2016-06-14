-- 4.11.16
-- Efficiency
-- items are computed on demand
--
twiceNonZeroes = map (2*) . filter (/=0)
twiceNonZeros [1,0,2,0,3,0]

head $ twiceNonZeros [1,0,2,0,3,0]

2 : (map (2*) (filter (/=0) [0,2,0,2,3,0[))

-- haskell lets you write code in a dumb way and write efficiently!
-- can write inefficiently first, then write efficiently
--
-- List comprehensions:
-- foldr
-- ** Python took it from Haskell
--
insert :: Integer -> [Integer] -> [Integer]
insert n [] = [n]
insert n m@ (m1:_) | n < m1 = n : m
-- m1 is the first element in the list
-- @ is combining like pattern (pattern matching), just as in MAKE

-- recursive definition
--
sum :: [Integer] -> Integer
sum [] = 0
sum (n:ns) = n + sum ns

-- generalizing sum
-- NOT ALWAYS using +, can try accumulate
--
accumulate :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
accumulate _ init [] = init
accumulate f init (n: ns) = f n (accumuate f init ns)

-- structure of this function is EXACTLY the samea s sum function, with sum replaced by accumulate f init ns
-- now doesn't have to specify integer
-- 
accumulate :: (a -> a -> a) -> a -> [a] -> a

foldr
-- is a function of 2 arguments
-- TAKES operator, initial value, list
-- RETURNS computation 

concat
-- TAKES list of lists
-- RETURNS single list
 
map f = foldr (\x r -> f x : r) []
map f = foldr ((:) -- to be continued



