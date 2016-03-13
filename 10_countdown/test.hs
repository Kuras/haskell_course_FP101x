module HW10 where

import System.CPUTime
import Numeric
import System.IO
-- solve the countdown problem

-- formalising the rules

-- the simples solution
-- XP (bottom -> up process to resolve problem)
-- Operators:

-- Algebraic data type
data Op = Add | Sub | Mul | Div

-- we doing symbolic expresion
apply           :: Op -> Int -> Int -> Int
apply Add x y   = x + y
apply Sub x y   = x - y
apply Mul x y   = x * y
apply Div x y   = x `div` y 

-- roles
valid           :: Op -> Int -> Int -> Bool
valid Add _ _   = True
valid Sub x y   = x > y
valid Mul _ _   = True
valid Div x y   = x `mod` y == 0


-- expresion
data Expr = Val Int | App Op Expr Expr

-- Parser, => [] either [x]
-- recursive (1 * 70) + (34 / 5) = 777
eval                :: Expr -> [Int]
eval (Val n)        = [n | n > 0]
eval (App o l r)    = [apply o x y | x <- eval l
                                   , y <- eval r 
                                   , valid o x y]
     
-- Formalising the problem
-- helper function
choices     :: [a] -> [[a]]
choices []      = [[]]
choices (x:xs)  = [[x]] ++ choices xs

choices1 :: Eq a => [a] -> [[a]]
choices1 []  = [[]]
choices1 xs  =                   
                    choices1 (take (length xs -1) xs)
                    ++ choices1 (take (length xs -1) (reverse xs))
                    ++ [xs] 
                    ++ [reverse xs]

choices2 :: Eq a => [a] -> [[a]]
choices2 []  = [[]]
choices2 xs  = [take (length (takeWhile (\y -> y /= xp) xs)) xs | xp <- xs]

-- [[],[1],[2],[1,2],[2,1]]
-- not (elem 1 [1,2])
choices3            :: Eq a => [a] -> [[a]]
choices3 []         = [[]]
choices3 xs     = zipDiff (growingList xs) (foldingList xs)
                    where
                        growingList xs' = [takeWhile (\y -> y /= x) xs' | x <- xs'] ++ [xs']
                        foldingList xs' = [reverse ys | ys <- (growingList xs')]
                        zipDiff xss yss = [if xs /= reverse ys then ] 
                
values              :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r 

solution            :: Expr -> [Int] -> Int -> Bool
solution e ns n     = elem (values e) (choices ns)
                      && eval e == [n]

-- brute force aproach
split :: Eq a => [a] -> [([a], [a])]
split xs    = tail [(takeWhile (\y -> y /= xp) xs,dropWhile (\y -> y /= xp) xs) | xp <- xs]

exprs       :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
-- exprs ns    = [e | (ls,rs) <-]                    
-- improv