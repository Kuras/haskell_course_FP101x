module HW10 where

import System.CPUTime
import Numeric
import System.IO

-- Solve the countdown problem
-- Formalising the rules

-- The simples solution
-- XP (bottom -> up: process to resolve problem)


-- Algebraic data types {Op, Expr}
-- Operators:
data Op = Add | Sub | Mul | Div
instance Show Op where
   show Add                   =  "+"
   show Sub                   =  "-"
   show Mul                   =  "*"
   show Div                   =  "/"
   
apply           :: Op -> Int -> Int -> Int
apply Add x y   = x + y
apply Sub x y   = x - y
apply Mul x y   = x * y
apply Div x y   = x `div` y 
-- We doing symbolic expresion

-- Roles of game
valid           :: Op -> Int -> Int -> Bool
valid Add _ _   = True
valid Sub x y   = x > y
valid Mul _ _   = True
valid Div x y   = x `mod` y == 0


-- Expresion
data Expr = Val Int | App Op Expr Expr
instance Show Expr where
   show (Val n)               =  show n
   show (App o l r)           =  bracket l ++ show o ++ bracket r
                                 where
                                    bracket (Val n) = show n
                                    bracket e       = "(" ++ show e ++ ")"
eval                :: Expr -> [Int]
eval (Val n)        = [n | n > 0]
eval (App o l r)    = [apply o x y | x <- eval l
                                   , y <- eval r 
                                   , valid o x y]     
-- Parser -> [] | [(x)] == Maybe 
-- Recursive see in this expr -> {(25-10) * (50+1) = 765}
--      -> Space Solutions for 765 -> 780
-- No solutions -> --- = 831
--      -> Space Solutions for 831 -> 0
-- eval (App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1)))


     
-- Formalising the problem
-- Helper functions
-- [[],[1],[2],[1,2],[2,1]]
-- concat [[1,2], [2,1]]
-- cycle [id,(*2)]
-- zipWith (*) [1,2] [1,2]
-- zipWith (*) [1..] [1,4,8,10]
subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

permutate                     :: [a] -> [[a]]
permutate []                  =  [[]]
permutate (x:xs)              =  concat (map (interleave x) (permutate xs))

-- Exe 0
-- All posible ways choicing 0 or more
choices                       :: [a] -> [[a]]
choices xs  =  [zs | ys <- subs xs, zs <- permutate ys]

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

choices3            :: Eq a => [a] -> [[a]]
choices3 []         = [[]]
choices3 xs     = zipDiff (growingList xs) (foldingList xs)
                    where
                        growingList xs' = [takeWhile (\y -> y /= x) xs' | x <- xs'] ++ [xs']
                        foldingList xs' = [reverse ys | ys <- (growingList xs')]
                        zipDiff xss yss = xss ++ yss
                
values              :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r 
-- (25-10) * (50+1) -> [25,10,50,1]
-- values (App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1)))


solution            :: Expr -> [Int] -> Int -> Bool
solution e ns n     = elem (values e) (choices ns)
                      && eval e == [n]

-- Brute force aproach
-- Searching all posible solutions
-- Exe 1
split :: Eq a => [a] -> [([a], [a])]
split xs    = tail [(takeWhile (\y -> y /= xp) xs,dropWhile (\y -> y /= xp) xs) | xp <- xs]

-- Generate all expresons for given list of values
-- Brute forece way to generate all posible expresions for list f.e [25,10,50,1]
exprs       :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls,rs)  <- split ns
                 , l        <- exprs ls
                 , r        <- exprs rs 
                 , e        <- combine l r]                    
                 
combine     :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Sub,Mul,Div]]

solutions                     :: [Int] -> Int -> [Expr]
solutions ns n                =  [e | ns' <- choices ns
                                    , e   <- exprs ns'
                                    , eval e == [n]]
-- solutions [1,3,7,10,25,50] 765


-- Improve 1-> The fastes fail aproach
-- Never generate expr which is invalid
-- Now mechanizm go throught path even if path is wrong, but what we can do, we can stop this special path!!

type Result = (Expr, Int)
results     :: [Int] -> [Result]
-- results ns  = [(e,n) | e <- exprs ns
--                      , n <- eval e]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = 
    [res | (ls,rs)  <- split ns  
         , lx       <- results ls
         , ry       <- results rs
         , res      <- combine' lx ry]
            where 
                combine' :: Result -> Result -> [Result]
                combine' (l,x) (r,y) =
                    [(App o l r, apply o x y)
                        | o <- [Add,Sub,Mul,Div]
                        , valid o x y]

solutions'                     :: [Int] -> Int -> [Expr]                        
solutions' ns n                =
    [e | ns'    <- choices ns
       , (e,m)  <- results ns'
       , m == n]
-- Fuse generate exprs with theirs value




-- Improve 2-> symetrics
-- x * y = y * x
-- x * 1 = x

valid'           :: Op -> Int -> Int -> Bool
valid' Add x y   = x <= y
valid' Sub x y   = x > y
valid' Mul x y   = x <= y && x /= 1 && y /= 1
valid' Div x y   = x `mod` y == 0 && y /= 1


results'     :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = 
    [res | (ls,rs)  <- split ns  
         , lx       <- results' ls
         , ry       <- results' rs
         , res      <- combine'' lx ry]
            where 
                combine'' :: Result -> Result -> [Result]
                combine'' (l,x) (r,y) =
                    [(App o l r, apply o x y)
                        | o <- [Add,Sub,Mul,Div]
                        , valid' o x y]

solutions''                     :: [Int] -> Int -> [Expr]                        
solutions'' ns n                =
    [e | ns'    <- choices ns
       , (e,m)  <- results' ns'
       , m == n]
       
-- Whooooa extremely fast!!!


main = putStrLn "main"