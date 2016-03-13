module Main where

-- import Prelude hiding (Bool (..))

--- develop a simple abstract machine ---

-- define a type class
        -- instantiate

-- commonalitis with wtih OOP --
--    common feauter

-------- aliase --------
type String' = [Char]

type Pos = (Int,Int)
origin      :: Pos
origin      = (0,0)

left        :: Pos -> Pos
left (x,y)  = (x-1,y)

type Pair a = (a,a)
multi      :: Pair Int -> Int
multi (m,n) = m*n

copy        :: a -> Pair a
copy x      = (x,x)

-- type not recursive

-------- data , new type--------
-- data Bool' = False | True
-- False, True extends Bool' (java)
-- False, True -> constructors
-- algebraic data type

-- algebraic data type
-- c: abstract data type: ADT: Struct *struct
data Answer = Yes | No | Unknown

answers        :: [Answer]
answers        = [Yes,No,Unknown]

flip           :: Answer -> Answer
flip Yes       = No
flip No        = Yes
flip Unknown   = Unknown
-- general 'not'

-- OO example

data Shape = Circle Float
            |Rect Float Float

square            :: Float -> Shape
square n          = Rect n n

-- algebraic data type
--       * haskell ->
--       * java   -> obj. hierrchy
-- pater matching == dynamic binding in java
area              :: Shape -> Float
area (Circle r)   = pi * r^2
area (Rect x y)   = x * y

-- maybe type
-- like list either empty or single value [(x,S)]  =  Maybe
{-
data Maybe' a = Main.Nothing | Main.Just a
---------with---------- data Maybe a = Just a | Nothing deriving (Show) 
safediv        :: Int -> Int -> Maybe' Int
safediv  _ 0   = Main.Nothing
safediv  m n   = Main.Just (m `div` n)

safehead       :: [a] -> Maybe' a 
safehead []    = Main.Nothing
safehead xs    = Main.Just (head xs)

-}


--------- recursive type ---------
-- natural nambers
data Nat = Zero | Succ Nat
-- succesor
-- Zero::Nat
-- Succ::Nat -> Nat

-- Bottom = undefined
-- Zero
-- Succ Zero
-- Succ (SuccZero)
{-
Succ    (Succ   (Succ   Zero))
1 +     (1 +    (1 +    0))
-}

-- => the same structure
--      => homeomorfizm Nat -> Int

nat2int             :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat     :: Int -> Nat
int2nat 0   = Zero
int2nat n   = Succ (int2nat (n-1))

instance Show Nat where
    show Zero   = show 0
    show (Succ n) = show n
-- INVERS FUNCTION  
-- => nat2int (int2nat 10)
-- Nat is isomorphic to Int


add         :: Nat -> Nat -> Nat
-- not efficient
-- add m n     = int2nat (nat2int m + nat2int n)

add Zero     n = n 
add (Succ m) n = Succ (add m n)
-- int2nat n + 1   = Succ (int2nat (n))

{- use
add (Succ (Succ Zero)) (Succ Zero) 
-}



--------- type classes ---------
-- expresion

-- define algebraic recursive data type represent tree!!!

data Expr   = Val Int
            | Add Expr Expr
            | Mul Expr Expr
            
-- 1 + 2 * 3 == tree
-- Add (Val 1) (Mul (Val 2) (Val 3))

-- expresion -> define recursive fun
-- OOP size interface of abstract class, and implemet for concrete
size                        :: Expr -> Int
size (Val n)                = 1 
size (Add x y)              = size x + size y
size (Mul x y)              = size x + size y

eval                        :: Expr -> Int
eval (Val n)                = n 
eval (Add x y)              = eval x + eval y
eval (Mul x y)              = eval x * eval y   
            
-- binary tree

data Tree'   = Leaf Int
             | Node Tree' Int Tree'


             
-- define recursive structure of binary tree
occurs                  :: Int -> Tree' -> Bool
occurs m (Leaf n)       = m==n
occurs m (Node l n r)   = m==n 
                          || occurs m l
                          || occurs m r
                          
                          
flatten                 :: Tree' -> [Int]
flatten (Leaf n)        = [n]
flatten (Node l n r)    = flatten l
                          ++ [n]
                          ++ flatten r

-- tree is 'search tree' 
{-
<=>
    flatten -> sorted list
-}

occurs' m (Leaf n)       = m==n
occurs' m (Node l n r)  | m == n    = True
                        | m < n     = occurs' m l 
                        | n > m     = occurs' m r


main :: IO ()
main = putStrLn "haskell"