foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr f init (x:xs) = f x (foldr f init xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs
