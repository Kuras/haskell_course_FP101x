module HW11 where

-- --- Jam Session ---
-- Generalised Abstract Nonsense
-- Category Theory is mathematical way doing OOP, desing patterns
-- Category
--      build
--          * obj.
--          * morphism
-- Category of categories
--      build
--          * category
--          * functor
-- Category of functor
--      build
--          + functors

-- Exercise 0  --

{-
To what would you (directly) reduce it to?
1 + (2 * 3)

=> 2 * 3
-}

-- Exercise 1  --

{-
sq n = n*n
sq (3+4) 
(3+4) * (3+4)



(1 + 2) * (2 + 3)

=> 1 + 2
=> 2 + 3s

-}

-- Exercise 2  --

{-
-> left
fst (1 + 2, 2 + 3)

redex
    innermost
        1+2, 2+3
    outermost
        fst (1 + 2, 2 + 3)
        
-}

-- Exercise 3 --

{-
-> left
(\x -> 1 + x ) (2 * 3)

redex
    innermost
        2 * 3
    outermost
        (\x -> 1 + x ) (2 * 3)
        
-}

-- Exercise 4  --

{-
-> left
fst (1 + 2, 2 + 3)

redex
    innermost
        fst (1 + 2, 2 + 3)
        = {applying the first +}
        fst (3, 2 + 3)
        = {applying fst}
        3

    outermost
        fst (1 + 2, 2 + 3)
        = {applying the fst}
        1+2
        = {applying +}
        3
-}

-- Exercise 5  --

{-
-> left
mult = \x -> (\y -> x * y )
mult 3 4

3 * 4
(\x -> x * 4) 3
(\x -> (\y -> x * y) 3 4

mult = \x -> (\y -> x * y)

-}

-- Exercise 6  --
-- oo Fibonacci numbers ([0, 1, 1, 2, ...]).

fibs    :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

-- Exercise 7  --
--  returns the n-th Fibonnaci number
fib :: Int -> Integer
fib n = fibs !! n


-- Exercise 8  --
--  the first Fibonacci number greater than 1000
largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)


-- Exercise 9  --
data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
                deriving Show
{-
repeat          :: a -> [a]
repeat x        = xs
    where xs    = x : xs

    => oo seq
-}

repeatTree          :: a -> Tree a
repeatTree a        = Node t a t
    where t = repeatTree a



-- Exercise 10  --
-- Innermost reduction
--  what??
-- May require fewer steps than outermost reduction