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

atom        :: IO a -> Concurrent a
atom ioa    = Concurrent (\c -> Atom (ioa >>= \a -> return (c a)))


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
                -> Using Fork data constructor
    
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

-- so we wona use Fork constructor so
-- fork' = Fork
-- fork' (const Stop)
-- Show ((() -> Action) -> Action)


fork'       :: ((a -> Action) -> Action) -> ((() -> Action) -> Action)
fork' ma    = (\x -> Fork (action' ma) (x ()))

fork        :: Concurrent a -> Concurrent ()
fork ca     = Concurrent (\x -> Fork (action ca) (x ()))

par         :: Concurrent a -> Concurrent a -> Concurrent a
par ca cb   = Concurrent (\x -> Fork (action ca) (action cb))


-- ===================================
-- Ex. 4
-- Don't try to understand what the code does operationally, trust the types.
-- Haskell only let you done things: "just one way to wire up all the pieces"
-- ===================================

{-
ma :: ((a -> Action) -> Action)
f  :: (a -> ((b -> Action) -> Action))

create value/result of tyoe
((b -> Action) -> Action)
-}
{-
\c -> ... expression of type Action ...                     :: ((b -> Action) -> Action)
\a -> ... expression of type ((b -> Action) -> Action) ...  :: a -> ((b -> Action) -> Action)

z :: Num a => a -> a -> a
z =  \x ->(\z -> z+x+1)
-}

bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
bind ma f = \t -> (ma (\a -> (f a) t))

-- bad 
-- (Concurrent f) >>= g = Concurrent(\t -> (f (\a -> (action(g a)) t)))

-- g a :: a -> Concurrent a
-- so
-- case g a of
--          Concurrent x -> x  <- take value out

{-
add the boilerplate pattern matching
    case g a of
applications of Concurrent
    Concurrent ()
-}
instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent(\t -> (f (\a -> case g a of
                                                            Concurrent x -> x t)))
    return x = Concurrent (\c -> c x)


instance Functor Concurrent where
    fmap = liftM
instance CA.Applicative Concurrent where
    pure  = return
    (<*>) = ap
-- ===================================
-- Ex. 5
-- ===================================

roundRobin          :: [Action] -> IO ()
roundRobin []       = return ()
roundRobin (x:xs)   = case x of
                            Atom a      -> do { a' <- a; roundRobin (xs ++ [a']) }
                            Fork a b    -> roundRobin (xs ++ [a,b])
                            Stop        -> roundRobin xs    
                            

-- ===================================
-- Tests

--  Run our Concurrent Monad
--     It is now time to put everything together 


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

-- Exercise ... --
-- run $ atom (putStrLn "a") >> atom (putStrLn "b") >> atom (putStrLn "c")

-- putStrLn "a" >> putStrLn "b"  :: ()
ex2 :: Concurrent ()
ex2 = h "a" >> h "b" >> h "c"
        >> atom (putStrLn "")
            where h s = atom (putStr s)
            
ex3 :: Concurrent ()
ex3 = fork (h "a'" >> h "b'" >> h "c'")
        >> (h "a" >> h "b" >> h "c")
        >> atom (putStrLn "")
            where h s = atom (putStr s)
            
ex4 :: Concurrent ()
ex4 =      (h "a" >> h "b" >> h "c")
        >> (h "a" >> h "b" >> h "c")
        >> atom (putStrLn "")
            where h s = atom (putStr s)
            
ex5 :: Concurrent ()
ex5 = par  (h "a'" >> h "b'" >> h "c'")
           (h "a" >> h "b" >> h "c")
        >> h ""
        >> h "..."
        >> h "end"
            where h s = atom (putStr s)
