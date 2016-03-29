module HW12 where

-- Exercise 0 --
-- overlapping patterns 

last1 :: [a] -> a
last1 (_ : xs) = last1 xs
last1 [x] = x

{-
exe.hs:7:1: Warning:
    Pattern match(es) are overlapped
    In an equation for ‘last1’: last1 [x] = ...
    
    last1 [1..10]
    => Non-exhaustive patterns in function last1
    
--> some case branch is not reacheable!!    
--> so must be sure that put def. in right order

last1 (_ : xs) = last1 xs
last1 [x] = x
because, patern matching goes from top to down!!!

last, init, drop

-}

last1' :: [a] -> a
last1' [x] = x
last1' (_ : xs) = last1' xs
--
foldr1'              :: (a -> b -> b) -> b -> [a] -> b
foldr1' _ v []       = v
foldr1' f v (x:xs)   = f x (foldr1' f v xs)

foldr1''              :: (a -> b -> b) -> b -> [a] -> b
foldr1'' f v (x:xs)   = f x (foldr1'' f v xs)
foldr1'' _ v []       = v
--
init1'          :: [a]->[a]
init1' [_]      = []
init1' (x:xs)   = x : init1' xs

init1''          :: [a]->[a]
init1'' (x:xs)   = x : init1'' xs
init1'' [_]      = []


-- Exercise 1 --
-- Proof: add n (Succ m) = Succ (add n m), by induction on n
-- 3

-- Exercise 2 --
-- addition is commutative
-- add n Zero = n
-- add (Succ n) m = Succ (add n m)
-- add Zero m = m
-- Proof: add n m = add m n, by induction on n
--3

-- Exercise 3 --
-- Proof: replicate [x..], by induction on n >= 0
-- all {all (\x -> (x `mod` 2) == 0) [2,4]}

-- all (== x) (replicate n x)

-- 1

-- Exercise 4 --
-- Proof: xs ++ [] = xs, by induction on xs
-- 4

-- Exercise 5 --
-- Proof: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs ,by induction on xs
-- Induction hipothesis : xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- 1

-- Exercise 6 --
-- Proof: map f (map g xs) = map (f . g) xs, by induction on xs
-- 3

-- Exercise 7 --
-- Proof: length (xs ++ ys) = length xs + length ys, by structural induction over the list xs
-- 2

-- Exercise 8 --
-- 4

-- Exercise 9 --
-- Proof:  length (take n (repeat x)) = n
-- 2




































