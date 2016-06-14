--
-- CS 115
-- Last Lab (Lab 6)
-- Kai Chang
--

import Control.Monad as CM
import Control.Monad.State as CMS
import Data.IORef as Data

-- I started early / got a lot of help for this one. Not really sure how to do Part B though..


-- Part A
--


-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)


-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)

-- Question 1
factIO :: Integer -> IO Integer
factIO n | n < 0 = error "Value less than zero."
factIO n = do
       total <- newIORef 1 
       v <- newIORef n 
       whileIO
        (do count <- readIORef v
            return (count /= 0))
        (do val <- readIORef total
            count <- readIORef v
            writeIORef total (val * count) 
            writeIORef v (count - 1)) 
       readIORef total

-- Question 2
factState :: Integer -> Integer
factState n | n < 0 = error "Value less than zero."
factState n = evalState factor (n, 1)
	where
                factor :: State (Integer, Integer) Integer
                factor = do
                       whileState
                        (\(n, _) -> n /= 0)
                        (do (n, i) <- get
                            put (n-1, i*n))
                       (_, i) <- get
                       return i

-- Question 3
fibIO ::  Integer -> IO Integer
fibIO n | n < 0 = error "Value less than zero."
fibIO n = do
      n1 <- newIORef 0
      n2 <- newIORef 1
      count <- newIORef n
      whileIO
        (do i <- readIORef count
            return (i /= 0))
        (do sec <- readIORef n1
            lst <- readIORef n2
            i <- readIORef count
            writeIORef n1 lst
            writeIORef n2 (sec + lst)
            writeIORef count (i - 1))
      readIORef n1

-- Question 4
fibState :: Integer -> Integer
fibState n | n < 0 = error "Value less than zero."
fibState n = evalState fib (0, 1, n)
         where
                fib :: State (Integer, Integer, Integer) Integer
                fib = do
                    whileState (\(_,_,n) -> n/=0)
                               (do (fib1, fib2, n) <- get
                                   put (fib2, fib1 + fib2, n-1))
                    (v, _, _) <- get
                    return v


-- Part B

{-
Given the 3 functions (need the three because our >>= operator has g, f, and h)
f :: (a, r) -> b
g :: (b, r) -> c
h :: (a, r) -> c
h (x, r) =
   let y = f (x, r)
       z = g (y, r)
   in (z, r)

and (a, r) -> b = a -> r -> b
    (b, r) -> c = b -> r -> c
    (a, r) -> c = a -> r -> c

and using data Reader r * = Reader (r -> *), then we can write the characteristic monad functions in the reader monad as a -> Reader r *, meaning

f :: a -> Reader r b
f x = Reader (\r -> f (x, r))
g :: b -> Reader r c
g x = Reader (\r -> g (x, r))
h :: a -> Reader r c
h x = Reader (\r -> h (x, r))

which (Reader r) = m for a -> m *. Now,

h = f >=> g
h x = f x >>= g
g x >>= f = h

so then

h x = f x >>= g = Reader (\r -> h (x, r))
                = Reader (\r -> 
                   let y = f (x, r)
                       z = g (y, r)
                   in (z, r))

h x = Reader (\r ->
           let Reader (\g -> x) = (Reader r) x
           Reader (\h -> f g r) = f g r
           in h r)

h = Reader (\r ->
           let Reader (\g -> x) = (Reader r) x
           Reader (\h -> f g r) = f g r
           in h r)

g x >>= f = Reader (\r ->
                            let Reader (\g -> x) = (Reader r) x
                                Reader (\h -> f g r) = f g r
                            in h r)

(Reader r) x >>= f = Reader (\r ->
                            let Reader (\g -> x) = (Reader r) x
                                Reader (\h -> f g r) = f g r
                            in h r)

(Reader r) x >>= f = Reader (\r ->
                            let (Reader g) = (Reader r) x
                                Reader (\h -> f g r) = f g r
                            in h r)

(Reader r) x >>= f = Reader (\r ->
                            let (Reader g) = (Reader r) x
                                (Reader h) = f g r
                            in h r)

(Reader r) x >>= f = Reader (\r ->
                            let (Reader g) = (Reader r) x
                                x = g r
                                (Reader h) = f x
                            in h r)
mx >>= f = Reader (\r ->
                 let (Reader g) = mx
                 x = g r
                     (Reader h) = f x
                 in h r)
        = Reader (\r ->
                 let x = runReader mx r in
                     runReader (f x) r)
-}




-- Deriving return
{-
reader :: (a, r) -> a
reader (x, r) = x

which can be curried to 

reader2 :: a -> r -> a
reader2 x r = x
reader2 x = \r -> x

which because a -> r -> a is equivalent to a -> (r -> a)
and defining Reader as data Reader r a = Reader (r -> a)
then we can replace the following above as

reader_monad :: a -> Reader r a 
reader_monad x = Reader (\r -> x)

and thus because return must conceptually be the identity function in the monad, then

return :: a -> Return r a 
return x = Reader (\r -> x)


-}


