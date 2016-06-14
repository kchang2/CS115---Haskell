-- can only use letters in variable form
add :: Integer -> Integer -> Integer
add x y = x + y

-- can only use @!%#$ in operator form, which is denoted by ( )
(@#) :: Integer -> Integer -> Integer
(@#) x y = x+y - x

map2 :: (a -> b) -> [a] -> [b]
map2 f []	= []
--map2 f [x]	= [f x]
map2 f (x:xs) = f x : map2 f xs

--($$) :: [a] -> [a] -> [a]
--($$) [] ys = ys
--($$) (x:xs) ys = x : (xs ++ ys)


reverse :: [a] -> [a]
reverse xs = iter xs []
 where
    iter :: [a] -> [a] -> [a]
    iter [] ys = ys
    iter (x:xs) ys = iter xs (x:ys)