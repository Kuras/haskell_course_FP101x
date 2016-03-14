module Exercise where

import Test.QuickCheck
-- fast-and-loose reasoning

-- Exercise 1 --
removeone           :: Eq a => a -> [a] -> [a]
removeone x []      = []
removeone x (y : ys) 
  | x == y = ys
  | otherwise = y : removeone x ys

-- Exercise 2 --
isChoice            :: Eq a => [a] -> [a] -> Bool
isChoice [] _       = True
isChoice (x:xs) []  = False
isChoice (x:xs) ys  = elem x ys && isChoice xs (removeone x ys) 
-- Exercise 3 -- 

-- quickcheck
split'          :: [a] -> [([a],[a])]
split' []       = []
split' [_]      = []
split' (x:xs)   = ([x],xs) : [(x : ls,rs) | (ls,rs) <- split' xs]         

-- Property based testing seems very interesting
-- Quick Check -> Level higher than unit tests

-- This is QuickCheck 2, a library for random testing of program properties.

-- Distributivity law 
-- reverse , ++
-- Easier to reason about
--          => reverse (xs++ys) = reverse ys ++ reverse xs
-- Property holds => high-school algebra + induction

{- -- This is not perfect proof. But show like it may looks like in haskell.
(1)Formal proof of property (++, reverse), "reverse (xs++ys) == reverse ys ++ reverse xs".

Let's see that reverse [] == [], and reverse [x] == [x] for any x.
Given list  [x,x'], then we could compose list like that [x] ++ [x'] for any x,x'.
So now we have reverse [x] ++ reverse [] == [x] ++ [] ==  reverse ([] ++ [x]), and for 
[x] ++ [x'] == reverse [x] ++ reverse [x'] == [x,x'] == reverse ([x',x]) == reverse ([x'] ++ [x]).

So from mathematicaly induction we have, let's
[x_1..x_n] == [x_1..x_n-1] ++ [x_n] == reverse [x_n] ++ reverse [x_1..x_n-1] == reverse [x_n..x_1]
Then 
reverse ([x_1..x_n] ++ [x_n+1]) == reverse ((reverse [x_n..x_1]) ++ [x_n+1]) == reverse [x_n+1] ++ reverse ((reverse [x_n..x_1]))

Maybe instead proof property for all posible value we are going run rundom 100 tests for that!!!
 => (reverse , ++) => 
        => Quick Check (reverse , ++)
-} 


-- Expressing properties about programs and testing them with randomly-generated values.
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys
-- Counter-example is 
--          shrinking heuristic => the simples counter example
--                                        => debugging our programs


-- Combine functors
-- let a = fmap (fst) [([2],[1,3,4]),([2,3],[1,4])]
-- fmap (\f -> 1: f) a
--      => Applicative functors

-- f1 g (xs,ys) = (g xs,ys)
-- fmap (f1 (1:)) [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-- fmap (\(xs,ys) -> (1:xs,ys) [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

putFirst x = fmap (\(xs,ys) -> (x:xs,ys))

prop_splitapp          :: Int -> [Int] -> Bool
prop_splitapp x ys     = split' (x:ys) == putFirst x (split'(ys))

main :: IO ()
main = do 
            quickCheck prop_revapp
            quickCheck prop_splitapp