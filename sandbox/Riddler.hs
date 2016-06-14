import Data.List

pairs :: [(Int, Int)]
pairs = [(i * j, i + j)] | i <- [1..9], j <- [1..9], i >=j]

-- nonUniques f pairs = map head $ filter (\lst -> length lst > 1) $ group $ sort $ map fst pairs)
-- nonUniques f = map head . filter (\lst -> length lst > 1) . group . sort . map f
nonUniques f = map head . filter ((>1) . length) . group . sort . map f
filterNU f lst = filter (\p -> f p 'elem' (nonUniques f lst)) lst
-- filterNU = join . (filter .) . ap ((.) . flip . (elem .)) nonUniques

-- map head $ filter (\lst -> length lst > 1) $ group $ sort $ map fst pairs
-- nonUniques sort f
-- pointfree gets rid of all the arguments, just put in your code and then it does what mathematica is "FullSimplify"
