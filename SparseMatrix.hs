module SparseMatrix where

-- IntMap a -> [(Key,a)] vs. Map k a -> [(k,a)]
-- Do you need (Eq a, Num a) => for operators?

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- Question 1
sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix _ (a,b) | a < 1 = error ("Undefined bounds")
                     | b < 1 = error ("Undefined bounds")
sparseMatrix xs (a,b) | filter (\x-> x <= 0 || x > a) (map (fst . fst) xs) /= [] = error "value not in bounds"
                  | filter (\y-> y <= 0 || y > b) (map (snd . fst) xs) /= [] = error "value not in bounds"
sparseMatrix x bo = SM {
             bounds = bo,
             rowIndices = S.fromList (map (fst . fst) xp),
             colIndices = S.fromList (map (snd . fst) xp),
             vals = (M.fromList xp)}
             where
                xp = filter (\(_,v)->v /=0) x

-- look into intersection with then add
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM dim1 _ _ _ ) (SM dim2 _ _ _ ) | dim1 /= dim2 = error ("dimensions of two matrix not identical")
addSM (SM dim _ _ v1) (SM _ _ _ v2) = sparseMatrix (M.toList (M.unionWith (+) v1 v2)) dim


-- Question 3
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c v) = SM {bounds = b, rowIndices = r, colIndices = c, vals = (M.map (negate) v)}

-- Question 4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM sm1 sm2 = addSM sm1 (negateSM sm2)


-- Question 5
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM (_,y1) _ _ _ ) (SM (x2,_) _ _ _ ) | y1 /= x2 = error ("dimensions of two matrix not identical for multiplying")
mulSM sm1@(SM (x,_) _ _ _) sm2@(SM (_,y) _ _ _) = sparseMatrix (lofnewV sm1 sm2 (1,1)) (x,y)
      where
        lofnewV :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> (Integer, Integer) -> [((Integer, Integer), a)] -- takes in the two sparse matrices, and then indice counter for recursion
        lofnewV sm1@(SM (x,_) _ _ _) sm2@(SM (_,y) _ _ _) (r,c) | r > x = []
                                                                  | c < y = ((r,c), rcmul (getRow sm1 r) (getCol sm2 c)) : (lofnewV sm1 sm2 (r, (c+1)))
                                                                  | c == y = ((r,c), rcmul (getRow sm1 r) (getCol sm2 c)) : (lofnewV sm1 sm2 ((r+1), 1))
                                                                  | otherwise = error "something unexpected occurred"

-- gets a list of [((r,c), v)] from filtering by row
getRow :: (Eq a, Num a) => SparseMatrix a -> Integer -> [((Integer, Integer), a)] -- assumes values all row values unique
getRow (SM _ _ _ v) row = M.toList (M.filterWithKey (\(r,_) _ -> r == row) v)

-- gets a list of [((r,c), v)] from filtering by column
getCol :: (Eq a, Num a) => SparseMatrix a -> Integer -> [((Integer, Integer), a)] -- assumes values all column values unique
getCol (SM _ _ _ v) col = M.toList (M.filterWithKey (\(_,c) _ -> c == col) v)

-- multiply two lists to make a ((r,c), v)
rcmul :: (Eq a, Num a) => [((Integer, Integer), a)] -> [((Integer, Integer), a)] -> a
rcmul [] _ = 0
rcmul (((_,c),v1):v1s) v2 | fv == [] = 0 + (rcmul v1s v2)
                        | otherwise = (v1 * (snd (head fv))) + (rcmul v1s v2)
                        where fv = (filter (\((row,_),_) -> row == c) v2)

-- Question 6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (x,y) _ _ _) (q, w) | q > x || w > y = error ("out of bounds of the Sparse Matrix")
getSM (SM _ _ _ v) k = M.findWithDefault 0 k v

rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (x,_) _ _ _) = x

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_,y) _ _ _) = y

-- all :: determines whether all elements of the structure satisfy the predicate


-- Question 7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) sm1 sm2 = addSM sm1 sm2

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) sm1 sm2 = subSM sm1 sm2

{-(<|*|>) :: SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) sm1 sm2 = mulSM sm1 sm2-}

(<|!|>) :: (Eq a, Num a) => SparseMatrix a -> (Integer, Integer) -> a
(<|!|>) sm1 a = getSM sm1 a

-- Question 8
-- It does not make sense to define the SparseMatrix datatype as an instance of the Num type class because it also needs Ordering for values, in order to make sense of row and columns.
-- Ah, so the reason why the Num type class doesn't make sense for the SparseMatrix is because
-- the Num type class has some functions that the SparseMatrix can fufill. For example, 
-- the fromInteger function doesn't make sense because you can't convert an integer into a matrix.

