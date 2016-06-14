--
-- CS 115
-- Lab 4a
-- Kai Chang
--

import Prelude as P
import System.Environment as SEN
import System.Exit as SEX

-- how the program is called upon in ghc: % reverse filename
-- compile using ghc -W -o reverse Reverse.hs

-- if too few arguments: usage: reverse filename, and exit with failure status
-- exitFailure
-- if correct and successful completion: exit with success status
-- exitSuccess

main :: IO()
main = do
     shit <- SEN.getProgName
     file <- SEN.getArgs
     if (length file) /= 1
        then putStrLn ("usage: " ++ shit ++ " filename") >> SEX.exitFailure
     else do
          f <- (P.readFile (head file)) -- gets the "first" file
          P.mapM_ (\poo -> (putStrLn poo)) (P.reverse (lines f)) >> SEX.exitSuccess


