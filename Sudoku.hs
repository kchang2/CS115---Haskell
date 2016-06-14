--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--
-- I realized my mistake when Sudoku is an IOArray..
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where

    -- I originally tried using where, but there were too many positions
    -- this moves on to next step in Sudoku puzzle
       increment :: (Int, Int) -> (Int, Int)
       increment (i,9) = (i + 1, 1)
       increment (i,j) = (i, j + 1)
       
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    -- used help from https://zuttobenkyou.wordpress.com/2011/03/01/my-newbie-experience-with-haskells-io-monad/
       iter :: Sudoku -> (Int, Int) -> IO Bool
       iter s c@(i,_) | i <= 9 = do
                                coord <- readArray s c -- needs to be stored because returns a monadic element
                                values <- getOKValues s c
                                if coord == 0 -- reads from charToInt, replacing all '.' with 0s
                                   then do
                                        valid <- iter' s c values
                                        if valid
                                           then iter s (increment c)
                                        else
                                           return False
                                else
                                   iter s (increment c)
                    | otherwise = return True

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    -- (Coordinates) -> Moves left -> Binary choice
       iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
       iter' _ _ [] = return False -- in IO needs return
       iter' s c (x:xs) = do
                              writeArray s c x -- writeArray returns () (ie. identical to IO ()), so it does not need to be stored. The array is a MArray or mutable array!
                              valid <- iter s c
                              if valid
                                 then iter s (increment c)
                              else do
                                   writeArray s c 0
                                   iter' s c xs

    -- Union of the rows values filled, columns value filled, and box value filled
    -- then takes the anti-intersection of these values
    -- I couldn't find the function form for it, just the operator, originally thought intersection, but that is incorrect.
       merge :: [Int] -> [Int] -> [Int] -> [Int]
       merge r c b = [0..9] \\ (union (union r c) b)


    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
       getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
       getOKValues s (i,j) = do
                             rows <- getRow s i
                             cols <- getCol s j
                             box <- getBox s (i,j)
                             return (merge rows cols box)

    -- Return the ith row in a Sudoku board as a list of Ints.
    -- Different from traditional lambda mapping,
    -- also not a filter (which I originally tried)
       getRow :: Sudoku -> Int -> IO [Int]
       getRow s i = mapM (readArray s) [(i,y) | y <- [1..9]] 
    -- getRow s i = mapM (readArray \(x,i) -> x <=9 && x >= 1) s 

    -- Return the ith column in a Sudoku board as a list of Ints.
       getCol :: Sudoku -> Int -> IO [Int]
       getCol s j = mapM (readArray s) [(x,j) | x <- [1..9]] 

    -- Return the box covering location (i, j) as a list of Ints.
    -- The box is the 3x3 that takes up all 81 spaces, but is distributed
    -- symmetrically amongst 9 boxes.
       getBox :: Sudoku -> (Int, Int) -> IO [Int]
       getBox s (i,j) = mapM (readArray s) [(i'+x,j'+y) | x <-[1..3], y <-[1..3]]
                        where
                                i' = (quot (i-1) 3) * 3
                                j' = (quot (j-1) 3) * 3

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

