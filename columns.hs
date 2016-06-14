import Prelude
import System.Environment
import System.Exit
-- import Control.Monad
-- import Data.Char
-- import Data.List
import System.IO

-- arrangement is such that
-- we have columns n1 n2 ... filename
-- in terminal, we have to do such that it runs % ./columns n1 n2 ... filename

-- checks to see if column value is > 0
checkCol :: (Num a, Ord a) => [a] -> Bool
checkCol [] = True
checkCol (x:xs) | x <= 0 = False
                | otherwise = checkCol xs

-- I originally tried filter, and then delete, but no avail... [char] and [string] type
-- error.
firstLast :: [a] -> [a]
firstLast xs = init xs

-- returns the correct items in a single line by column
getValues :: [Int] -> [String] -> [String]
getValues [] _ = []
getValues (c:cx) items | c > (length items) = getValues cx items
                       | otherwise = (items !! (c-1)) : (getValues cx items)

main :: IO ()
main = do
     file <- getArgs -- gets list of files
     f <- readFile (last file) -- reads filename
     if length file <= 1 -- ./columns is not considered a file, so no worries there
        then putStrLn ("No columns specified") >> exitFailure
     else if checkCol (map read (firstLast file)) == False
          then putStrLn ("Contains negative or Zero integer") >> exitFailure
     else
        let cols = map read (firstLast file) -- gets list of columns
        in do
           if f == "-" -- read from standard input or terminal
              then do
                   f2 <- hGetContents stdin
                   putStrLn (unwords (getValues cols (words (f2)))) >> exitSuccess
           else do -- read from filename
                mapM_ (\poo -> (putStrLn (unwords (getValues cols (words poo))))) (lines f) >> exitSuccess