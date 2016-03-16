module Lab5 where

-- -- Poor Man's Concurrency Monad -- --
-- implement a Monad for handling concurrency --

import Control.Monad
import qualified Control.Applicative as CA

data Concurrent a = Concurrent ((a -> Action) -> Action)
-- Suspend
--      => grab "future" and use then
--          imp : continuation => (a -> Action) -> Action

-- (a -> Action) -> Action into a monad
--      Concurrent a < trival algebracic data type
--      ignoring bottoms -> data


data Action = Atom (IO Action) | Fork Action Action | Stop

-- Atom -> primitive action
-- Fork -> concurrent exe of two action
-- Stop -> terminated action
instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"


-- ignoring the wrapper "Concurrent" (think like a fundamentalist)
--      then add pattern matching and constructor
-- ===================================
-- Ex. 0
-- ===================================

-- connection bet. Types <- morphism
{-
((a -> Action) -> Action)  -> trnsform -> Action
-}
action :: Concurrent a -> Action
action = error "You have to implement action"

action'             :: ((a -> Action) -> Action) -> Action 
action'             = \inp -> case inp of
                                \inp -> case inp of
                                        Stop        -> Stop



-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = error "You have to implement stop"


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork = error "You have to implement fork"

par :: Concurrent a -> Concurrent a -> Concurrent a
par = error "You have to implement par"


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


instance Functor Concurrent where
    fmap = liftM
instance CA.Applicative Concurrent where
    pure  = return
    (<*>) = ap
-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

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

