module HeapSort where -- declares module HeapSort, export all definition

import qualified PriorityQueue as Q -- importing all defintion from Priority

sort :: Ord a => [a] -> [a]
sort ls = helperSort (Q.fromList ls)
        where
                helperSort :: Ord a => Q.Pqueue a -> [a]
                helperSort a =
                           case (Q.popMin a) of
                                Just (e, s) = e : helperSort s
                                Nothing = []
