import Prelude as P
import System.Environment as SEN
import System.Exit as SEX
import Control.Monad as CM
import Data.Char as DC
import Data.List as DL
import Data.Maybe as DM
import System.IO as SI

checkCol :: [Integer] -> Bool
checkCol [] = True
checkCol (x:xs) | x < 0 = False
                | otherwise = True
