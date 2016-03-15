module HW11 where

-- Everywhere lazzzyyyyyy
--    => introduce infinite Algebraic data structure
--    => more modular
--    => avoid unnecessary evaluation

square n = n * n

-- square(3+4)
-- Evaluated and reduce:
-- Strategy 1
--    3+4 -> 7
--    7*7 -> 49

-- Strategy 2
-- Haskell avoid duplicate computation
--    (3+4) * (3+4) -> 49

-- Strategy1, Strategy2 evaluate expresion in diff. order
--  Side Efect
--      => so side efect may be diff. in this cases
--  Refactor easy
--      => more modular

loop :: [a]
loop = tail loop

-- fst (1, loop)
-- Evaluated and reduce
-- loop never ending story

-- Innermost reduction [3+4->7]
-- so first try reduce loop
-- oo(infinite) step reduction
-- fst (1, loop)=
  -- fst (1, tail loop)=
    -- fst (1, tail (tail loop))=
-- This strategy does not terminate!!!

-- Outermost reduction [(3+4) * (3+4)]
-- One step reduction
-- fst (1, loop)= 1
-- This strategy does terminate

-- From first exe
--      If E terminate seq(3+4->7) => 
--            outermost terminate. The same result

-- From snd exe
--      If outermost terminate => 
--            innermost may not terminate!


-- Strategy 3 (Lazy evaluation)
-- Outermost reduction + sharing
-- Sherring resoult
-- square (3+4)
-- (_*_) -> (3+4)
-- (_*_) -> 7       <= pointers to expresion!!! => one evaluated

-- Example
ones :: [Int]
ones = 1 : ones
-- oo (potentiall infinite)
-- take 5 ones

-- head ones
-- i) Innermost reduction
{-
head ones = head (1: ones)
          = head (1: (1: ones))
-}

-- ii) Lazy eval reduction
{-
head ones = head (1: ones)
          = 1
-}
--    lazy ==> Ruski telewizor

---------- Modular ----------
{-
take 5 ones
take 5 [1..]

take 5      <- control
ones/[1..]  <- data
-}

---------- Generator Primes ----------
-- Using Lazy

-- sieve of Eratosthenes --
primes    :: [Int]
primes    = sieve [2..]

sieve         :: [Int] -> [Int]
sieve (p:xs)  = p : sieve [x | x <- xs, x `mod` p /= 0]

-- take 10 primes
-- lazy 
--    => not compute all prime numbers, only compute 10!!
-- Modular 
--    take 10 primes
--    takeWhile (<15) primes

-- Imperative language
--    => We must compute this all prime numberss!!!!
--    => We not have this property
--    => Must fuze this two (take, primes) opereation in ONE
--          => break modularity

{-
length [1..4] > (-1)
length ones > (-1)
    -> Why the lazy evaluation is not triggered ?
    -> Lazy == do the minimal work, and length minimal work is oo
-}
