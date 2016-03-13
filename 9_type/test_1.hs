{-# OPTIONS_GHC -v #-}
---- Exercise 0 ----

-- total and terminating implementation


-- non-bottom, non-partial, finite natural number
-- 0 is a natural number according to this definition

module Main (main) where
import Data.List
import Data.Char
import Unsafe.Coerce
import qualified Control.Monad as CM
import Control.Monad (liftM, ap, mplus, mzero)
import qualified Control.Applicative as CA

-- import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
            | Succ Nat
            deriving Show

main = putStr "haskell"


natToInteger            :: Nat -> Integer
natToInteger (Succ n)   = natToInteger n + 1
natToInteger Zero       = 0

natToInteger'           :: Nat -> Integer
natToInteger'           = \n -> genericLength [c | c <- show n, c == 'S']


-- Exercise 1 --
-- >= 0
-- non-bottom
-- non-partial
-- finite

integerToNat        :: Integer -> Nat
integerToNat 0      = Zero
integerToNat n  = Succ (integerToNat (n-1))

-------- Exercise 2 ---------
add                :: Nat -> Nat -> Nat
add Zero        n  = n 
add (Succ m)    n  = Succ (add n m)


-------- Exercise 3 ----------

mult                :: Nat -> Nat -> Nat
mult m Zero         = Zero
mult m (Succ n)     = add m (mult m n)


-------- Exercise 4 ----------

{-
algebraic data type 
comparisons between

data Ordering = LT
              | EQ
              | GT

compare :: (Ord a) => a -> a -> Ordering
    decide
        if 
            x :: Ord a => a
            y :: Ord a => a

                => LT | EQ | GT

-}
data Tree   = Leaf Integer
             | Node Tree Integer Tree


occurs0                  :: Integer -> Tree -> Bool
occurs0 m (Leaf n)       = m==n
occurs0 m (Node l n r)  | m == n    = True
                        | m < n     = occurs0 m l 
                        | n > m     = occurs0 m r


-- tree is 'search tree' 
{-
<=>
    flatten -> sorted list
-}
{-
Tree
    finite
    non-partial
    non-bottom
    binary search tree
-}

occurs1                  :: Integer -> Tree -> Bool
occurs1 m (Leaf n)       = m == n
occurs1 m (Node l n r)
    = case compare m n of
            LT -> occurs1 m l 
            EQ -> True 
            GT -> occurs1 m r

occurs2                  :: Integer -> Tree -> Bool
occurs2 m (Leaf n)       = m == n
occurs2 m (Node l n r)
    = case compare m n of
            LT -> occurs2 m r 
            EQ -> True 
            GT -> occurs2 m l

occurs5                  :: Integer -> Tree -> Bool
occurs5 m (Leaf n)       = m == n
occurs5 m (Node l n r)
    | m == n    = True
    | m < n     = occurs5 m l 
    | otherwise = occurs5 m r
{- use
occurs1 1 (Node (Leaf 1) 3 (Leaf 5))


-}

-- ---------- Exercise 5 -- ----------

data Tree1   = Leaf' Integer
             | Node' Tree1 Tree1
             deriving Show

{-
Tree1 -> balanced
    if 
      differs (number of leaves in the left, number of leaves in the right) <= 1   
-}

balanced            :: Tree1 -> Bool

leaves (Leaf' _)     = 1
leaves (Node' l r)   = leaves l + leaves r
balanced (Leaf' _)   = True
balanced (Node' l r)
        = (abs (leaves l - leaves r) <= 1) && balanced l && balanced r

{- use
balanced (Node' (Leaf' 1) (Leaf' 5))
-}
  ------------------   Exercise 6      ----------


balanced1            :: [Integer] -> Tree1
-- list 
    -- non-bottom
-- integers 
    -- non-bottom

halve xs = splitAt (length xs `div` 2) xs
balanced1 [x]   = Leaf' x
balanced1 xs    = Node' (balanced1 ys) (balanced1 zs)
    where (ys,zs) = halve xs
{- use
balanced1 [1..4]
-}

{-
expresions
   * Add (Val 1) (Val 2)
   * Node (Leaf 1) (Leaf 2)
-}

--- Exercise 9 ---
-- When create mondas we must proof the
    -- Monad Laws

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap = liftM
instance CA.Applicative Maybe' where
  pure  = return
  (<*>) = ap

instance Monad Maybe' where
    return  x = Just' x
    Nothing' >>= _ = Nothing'
    (Just' x) >>= f = f x


----------------Exercise 10-------------------

-- concat [[1,2,3], [1,2,3]]


------------------- Exercise 11 -------------------
-- A monoid is an algebraic structure over a type a
        -- + single binary operation : (<>)     <- associative
        -- + neutral element : mempty           <- neutral


class Monoid' a where
    mempty' :: a
    (<>) :: a -> a -> a

instance Monoid' [a] where
    mempty' = []
    (<>)   = (++)

-- because we have operation ++ on list
    -- is binary operation
    -- and this operetion has neutral element []

--------------------Exercise 12-------------------------------
-- A functor
    -- type constructor with an operation
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    --  + (fmap f) . (fmap g) = fmap (f . g)    <- one cond
    --  + fmap id = id                          <- sec cond
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b


-- A value of type Functor f => f a    === "collection"
-- for exa. 
--  i)      f === []
--  ii)     f === Maype

instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just a) = Just (f a) 

------------------ Exercise 13 --------------------

class (Functor f) => Foldable' f where
    fold' :: (Monoid m) => f m -> m

instance Foldable' [] where
    fold' xs = concat . map (mempty)