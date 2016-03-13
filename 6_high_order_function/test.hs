all' p = and . map p
-- shortcut
-- all' p xs = and . map p xs

length' :: (Num b, Foldable t) => t a -> b
length' = foldr (\_ n -> 1 + n) 0

{-
length' ['a','b','c']
1) f ('c', 0) = 1 + 0 = 1
2) f ('b', 1) = 1 + 1 = 2
3) f ('a', 2) = 1 + 2 = 3
-} 

--dec2int [2, 3, 4, 5]
--[2, 3, 4, 5] = 2:(3:(4:(5:[])))

--1-(2-(-1))

--foldr
--    start from right
--foldl
--    start from left
--dec2int 2:(3:(4:(5:[])))    =
--                            = 2:(3:(4:(5:0))) 


--          overloaded function 
                    -- f x = x > 3

-- churc numerals
{-
Decimal 2

Binary 10

String "**"

type Church = (a -> a) -> a -> a
    instance
        c2i
        c2s
                sucesor fn
                a = 0

                number n = n appl fn 0

zero = \s z -> z
one  = \s z -> s z
two  = \s z -> s (s z)
refactor
two  = \s z -> (s . s) z (fun. composition)
two  = \s -> s . s     (i-ta reduction (lambda calculus))

e.g.

c2i x = x (+1) 0
x - churc reprezentation of nummerb <- function
sucessor (+1)
0 = 0
c2i two = (\s z -> s (s z)) (+1) 0
        = (+1) ((+1) 0)
        = 2
c2s x = x ('*':) ""  __ "****" = 5
c2s two = (\s z -> s (s z)) ('*':) ""
        = ('*':) ('*':"")
        = "**"

Add
x' = c2i x
y' = c2i y
x' + y' = c2i x + c2i y
        = x (+1) 0 + c2i y
        = x (+1) (c2i y)
        = x (+1) (y (+1) 0)
        /beta reduction/
        = (\s z -> x s (y s z)) (+1) 0
        = (add x y) (+1) 0
        = c2i (add x y)
----------lambda calculus-------------
7 * 7 + 4
(\s -> s * s + 4) 7
(\s -> (\z -> s * s + z) 7 4
(\s z -> s * s + z) 7 4

        = x (+1) (y (+1) 0)
        = (\s -> x s (y s 0)) (+1)
        = (\s -> \z -> x s (y s z)) (+1) 0
        = (\s z -> x s (y s z)) (+1) 0 {beta extraction}

add x y = \s z -> x s (y s z)

Mul
two     = \s -> s . s     (i-ta reduction (lambda calculus))
three   = \f -> f . f . f     (i-ta reduction (lambda calculus))
six   = \s -> (s . s) . (s . s) . (s . s))     (i-ta reduction (lambda calculus))

mul x y = \s z -> x (y s) z
        = \s z -> x . y
        
        =
        = \s z -> (x . y) s z
        = (x . y)

Power
pow x y = ?
x' = c2i x
y' = c2i y

x' ^ y' = (c2i x) ^ (c2i y)
        = (x (+1) 0) ^ (c2i y)  <---------- ?


two     = \s -> s . s     (i-ta reduction (lambda calculus))
three   = \f -> f . f . f     (i-ta reduction (lambda calculus))

2*2*2
two ^ three = eight
            = \s -> mul s (mul s s)
            = \s -> mul s (mul s s)

pow x y = \s z -> x (y s) z
-}
type Church a = (a -> a) -> a -> a
c2i x = x (+1) 0

squares 
    | n == 0 = []
    | n > 0  = n : squares (n-1)