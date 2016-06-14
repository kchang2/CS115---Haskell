--
-- CS 115
-- Lab 4a
-- Kai Chang
--

import Data.Char

-- QA.1
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = putChar c >> myPutStrLn cs


-- QA.2
-- No need for the do
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- QA.3
-- simple desugaring
-- #1 Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = putStr "Enter your name: " >> getLine >>= \name -> (putStr "Hello, " >> putStr name >> putStrLn "!")

-- complex desugaring
-- #2, Ask the user for his/her name, then print a greeting
greet2_1 :: IO ()
greet2_1 = putStr "Enter your name: " >> getLine >>= \name -> case name of
         name2 -> (putStr "Hello, " >> putStr name2 >> putStrLn "!")
         _ -> fail "empty string, Pattern match failure in do expression"
-- the complex desugaring does not behave differently from the simple desugaring.

-- QA.4
-- Need to import this to get the definition of toUpper:
--import Data.Char

-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
-- Simple desugaring
greet3 :: IO ()
 greet3 = putStr "Enter your name: " >> 
  getLine >>= \(n:ns) -> 
    let name = toUpper n : ns in 
      putStr "Hello, " >> 
      putStr name >> 
      putStrLn "!"
-- greet3 = putStr "Enter your name: " >> getLine >>= \(n:ns) -> (let name = toUpper n : ns in putStr "Hello, " >> putStr name >> putStrLn "!")

-- Complex desugaring
greet3_2 :: IO ()
greet3_2 = 
  putStr "Enter your name: " >> getLine >>= \name -> case name of
        (n:ns) ->
          let name' = toUpper n : ns in
            putStr "Hello, " >> putStr name' >> putStrLn "!"
        _ -> fail "Pattern match failure in do expression"

---- Yes, it does have an effect. If there isn't anything in the input for getLine (ie. blank), then we cannot break down
---- an empty string or list, so it would then match on an errror. This is complex desugaring. In the case of the simple
---- desugaring, this would not be the case. This would yield: Non-exhaustive patterns in lambda.






