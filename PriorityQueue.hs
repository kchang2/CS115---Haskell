--
-- Lab 2
-- Kai Chang, CS115
--
--

--
-- Hey, I never used empty, but I imagine you can replace all my Leaf with empty and then it will work?
-- Can call upon all methods before and use them in place of what I did for each method. However, I just didn't do it. I can if you want? Please don't.
--

module PriorityQueue(Pqueue, 
                     empty, 
                     isEmpty, 
                     insert, 
                     findMin,
                     deleteMin,
                     popMin,
                     fromList,
                     isValid)
where


-- Datatype for priority queues, parameterized around an element type a.
-- The element type should be an instance of the Ord type class.
-- a stored element
-- a positive integer value called the "rank" of the node
-- a left and right subheap, each of which is also a leftist heap
data Pqueue a = Leaf | Node a Int (Pqueue a) (Pqueue a)

-- An empty priority queue storing values of type a.
empty :: Pqueue a
empty = Leaf
-- empty a = Leaf (Nil 0 Nil Nil) Node (Nil 0 Nil Nil)


-- -- Return True if the queue is empty.
isEmpty :: Pqueue a -> Bool
isEmpty Leaf = True
isEmpty _ = False
-- isEmpty poo = poo == Empty

-- -- Returns integer rank of priority queue argument
rank :: Pqueue a -> Int
rank Leaf = 0
rank (Node _ b _ _) = b

-- -- takes two priority queues and merges them
merge :: Ord a => Pqueue a -> Pqueue a -> Pqueue a
merge Leaf q2 = q2
merge q1 Leaf = q1
merge m@(Node e1 _ h1L h1R) n@(Node e2 _ h2L h2R) | e1 < e2 = make e1 h1L (merge h1R n)
                                                  | otherwise = make e2 h2L (merge h2R m)
      where
        make :: Ord a => a -> Pqueue a -> Pqueue a -> Pqueue a
        make e Leaf n = Node e (rank n) n Leaf
        make e m Leaf = Node e (rank m) m Leaf
        make e m@(Node _ r1 _ _) n@(Node _ r2 _ _) | r1 < r2 = Node e (r1+1) n m
                                                   | otherwise = Node e (r2+1) m n

-- -- Insert an item into a priority queue.
insert :: Ord a => a -> Pqueue a -> Pqueue a
insert e q = merge (Node e 1 Leaf Leaf) q

-- -- Find the minimum-valued element in a priority queue if possible.
findMin :: Ord a => Pqueue a -> Maybe a
findMin Leaf = Nothing
findMin (Node e _ _ _ ) = Just e

-- -- Delete the minimum element from a priority queue if possible.
deleteMin :: Ord a => Pqueue a -> Maybe (Pqueue a)
deleteMin Leaf = Nothing
deleteMin (Node _ _ h1 h2) = Just (merge h1 h2)

-- -- Remove the minimum element if possible and return it, 
-- -- along with the rest of the priority queue.
popMin :: Ord a => Pqueue a -> Maybe (a, Pqueue a)
popMin Leaf = Nothing
popMin (Node e _ h1 h2) = Just (e, merge h1 h2)

-- -- Convert an unordered list into a priority queue.
fromList :: Ord a => [a] -> Pqueue a
fromList = foldr insert Leaf

-- -- Validate the internal structure of the priority queue.
isValid :: Ord a => Pqueue a -> Bool
isValid Leaf = True
isValid (Node e r h1 h2) = r > 0 && rank h1 >= rank h2 && r == (rank h2) + 1 && minie(findMin h1) e && minie(findMin h2) e && isValid h1 && isValid h2
        where
                minie :: Ord a => (Maybe a) -> a -> Bool
                minie Nothing _ = True
                minie (Just mn) e = e <= mn
