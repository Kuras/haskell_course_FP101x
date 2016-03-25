module Lab5 where

-- -- Poor Man's Concurrency Monad -- --
-- implement a Monad for handling concurrency --

import Control.Monad
import qualified Control.Applicative as CA

data Concurrent a = Concurrent ((a -> Action) -> Action)
-- (a -> Action) -> Action into a monad
--      ignoring bottoms -> data

{-
> newtype Parser a = P (String -> [(a,String)])
> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])
-}

data Action = Atom (IO Action) | Fork Action Action | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
{-
((a -> Action) -> Action)  -> (morphism)transform(connection) -> Action
-}

{-
Stop :: Action
\ a-> Stop :: r -> Action
const Stop :: b -> Action

const Stop (const Stop (const Stop (const Stop (const Stop (const Stop (3+4))))))
const Stop (const Stop )
(a -> Action) -> Action
=>
((a -> Action) -> Action) -> Action
const Stop (const Stop (const Stop ))
-}
ma                  :: (a -> Action) -> Action
ma f                 = (const Stop f)

action'             :: ((a -> Action) -> Action) -> Action 
action' ma            = (ma (const Stop))

{-
ma in this specyfic context means 
ma = (a -> Action) -> Action
test
instance Show (Concurrent x) where
    show (Concurrent x) = show x
-}
action :: Concurrent a -> Action
action (Concurrent ma) = ma (const Stop) 

-- ===================================
-- Ex. 1
-- - helper functions!!!
-- => easy accesabel
--      for me (in console)
--          Stop
--      for another fun's
--          action (Concurrent (\_ ->Stop))
--          action stop

-- ===================================
-- i) Think like fundementalist
stop'   :: (a -> Action) -> Action
stop'   = const Stop
-- ii) Wraped in Monad
stop :: Concurrent a
stop = Concurrent (const Stop)

{- use
action' stop'
action stop
-}
-- ===================================
-- Ex. 2
-- ===================================

atom' :: IO a -> Action
atom' x = Atom (return (Stop))

atom'' :: IO a -> (a -> Action)
atom'' x = \c -> Atom (return (Stop))
atom''' :: IO a -> ((a -> Action) -> Action)
atom''' x = \c -> Atom ( x >>= \a -> return (c a))

atom        :: IO a -> Concurrent a
atom x      = Concurrent (\c -> Atom ( x >>= \a -> return (c a)))


-- ===================================
-- Ex. 3
{-
Fork
    <- accses
            fork arguments
                i) an action
                ii)input continuation ()
    <- par
            combines 2 computations
    
Fork Stop (Atom (return Stop))
t0 -> a0
\x -> 1

how to modyfie to input fun
(a0 -> t0) -> t0)
\x -> (x 1)

and now easy to make
(() -> t0) -> t0)
\x -> (x ())
-}
-- ===================================

fork'       :: ((a -> Action) -> Action) -> ((() -> Action) -> Action)

-- so we wona use Fork constructor so
-- fork' = Fork
fork' ma    = (\x -> Fork (action' ma) (x ()))

-- fork' (const Stop)
-- Show ((() -> Action) -> Action)

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

