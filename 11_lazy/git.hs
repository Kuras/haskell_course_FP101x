module Lab5 where

import Control.Monad
import qualified Control.Applicative as CA
--instance Monad Concurrent where
--    (Concurrent f) >>= g = ...
--    return x = Concurrent (\c -> c x)

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- Ex. 0
action :: Concurrent a -> Action
action (Concurrent ma) = ma (\x -> Stop)

-- Ex. 1
stop :: Concurrent a
stop = Concurrent (\x -> Stop)

-- Ex. 2
atom :: IO a -> Concurrent a
atom x = Concurrent (\c -> Atom ( x >>= \a -> return (c a)))

-- Ex. 3
fork :: Concurrent a -> Concurrent ()
fork a = Concurrent (\x -> Fork (action a) (x ()) )

par :: Concurrent a -> Concurrent a -> Concurrent a
par a1 a2 = Concurrent (\x -> Fork (action a1) (action a2))

-- Ex. 4

unwrap (Concurrent x) = x

instance Monad Concurrent where
  (Concurrent f) >>= g = Concurrent (\c -> f (\x -> unwrap (g x) c))
  return x = Concurrent (\c -> c x)

instance Functor Concurrent where
    fmap = liftM
instance CA.Applicative Concurrent where
    pure  = return
    (<*>) = ap
  
-- Ex. 5
roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a:as) = case a of
  Atom a     -> do { a' <- a; roundRobin (as ++ [a']) }
  Fork a1 a2 -> roundRobin (as ++ [a1,a2])
  Stop       -> roundRobin as

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

-- synsem's examples (Lab5: Concurrency Examples):

myex0 = run $ (ho >> ho >> ho) >>
              (hi >> hi >> hi) >> atom (putStr "\n")
  where ho = atom (putStr "ho")
        hi = atom (putStr "hi")
        
myex1 = run $ fork (ho >> ho >> ho) >>
                   (hi >> hi >> hi) >> atom (putStr "\n")
  where ho = atom (putStr "ho")
        hi = atom (putStr "hi")

myex2 = run $ fork (put3 "ba") >> fork (put3 "di") >>
        put3 "bu" >> atom (putStr "\n")
  where put3 = sequence . take 3 . repeat . atom . putStr

myex3 = run $ par (put3 "ba") (put3 "di" >> stop) >>
              atom (putStr "\n")
  where put3 = sequence . take 3 . repeat . atom . putStr

-- LittleBigDej's example:

syracuse :: Int -> Concurrent Int
syracuse 1 = return 1
syracuse n = (print n) >>= syr >>= syracuse
  where print n = do atom $ putStr $ (show  n) ++ " " 
                     return n
        syr n | n `mod` 2 == 0 = return (n `div` 2)
              | otherwise      = return (3 * n + 1) 

ex6 :: Int -> Concurrent ()
ex6 n = do syracuse n
           atom $ putStrLn ""