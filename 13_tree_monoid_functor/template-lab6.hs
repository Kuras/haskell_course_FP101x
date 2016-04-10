------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------
import Prelude hiding (Monoid, Foldable, mappend, mempty, foldMap)
-- bottom does not exist
-- "fast-and-loose reasoning"

-- :> is a constructor written in infix notation
-- Infix constructors {start :,symbols}

-- Prefix, Infix, Postfix

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================
-- root (1 :> [2 :> [], 3 :> []]) = 1

root            :: Rose a -> a 
root (x:>xs)    = x 

children            :: Rose a -> [Rose a]
children (x:>xs)    = xs

construct = flip (:>) []

tree = 'x' :> map (flip (:>) []) ['a'..'x']
-- function f^-1 
tree1 = map (\(x:>xs) -> x) (map (flip (:>) []) ['a'..'x'])
tree' = 'x' :> map (\c -> c :> []) ['a'..'A']

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================
-- easy steps !
--    baby steps
size            :: Rose a -> Int
size (x:>xs)    = 1 + case xs of
                        []          -> 0
                        otherwise   -> foldl (+) 0 (map (\z -> size z) xs)  

leaves          :: Rose a -> Int
leaves (x:>xs)  = case xs of
                        []          -> 1
                        otherwise   -> foldl (+) 0 (map (\z -> leaves z) xs)  

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- apply a function uniformly to all elements in a list
--  map

-- apply a function uniformly to all the elements in a rose tree

-- apply a function uniformly to all the elements in data structure
--          => Functor type class
{-
    fmap that generalizes the map
    instance Functor [] where
        fmap = map
    
    class Functor f where 
        fmap :: (a -> b) -> f a -> f b
            
        -> instantiate f to []
-}
-- ===================================
-- instantiate Rose to (x:>xs)
-- a :> [Rose a]
-- use case
-- fmap (*2) (1:>[1:>[],1:>[],1:>[]])
instance Functor Rose where
  fmap f (x:>xs) = case xs of
                        []  -> (f x) :> []
                        _   -> (f x) :> (map (\z -> fmap f z) xs)

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- Exercise 9
-- \Why Rose a -> Rose b
f r = fmap head $ fmap (\x -> [x]) r

-- ===================================
-- Ex. 11-13
-- ===================================
{-
m type 
instantiate m to []

instance Monoid [] where
  mempty = []
  xs `mappend` ys = xs ++ ys
  
algebraic structure
 over type m 
 
(++) is an associative operation
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
    ->induction over structure list xs 
    (i)  base case
            [] ++ (xs ++ ys) = xs ++ ys = ([] ++ xs) ++ ys
    (ii) hipotise
            [(x:xs) ++ ys = x:(xs ++ ys)]
            Let (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
            Then (x:xs) ++ (ys ++ zs) =
                 x:(xs ++ (ys ++ zs)) =
                 x:((xs ++ ys) ++ zs) =
                 (x:(xs ++ ys)) ++ zs   =
                 ((x:xs) ++ ys) ++ zs
            
[] is indeed an identity element (++)
    ls, [] ++ ls = ls and ls ++ [] = ls
    Reductio ad absurdum
    
-}
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

-- Numbers also form a Monoid
{-
mempty = 0
x `mappend` y = x + y
    *addition with 0
    a = a + 0 = 0 + a 
    a + (b + c) = (a + b) + c 
    *multiplication with 1
    a = a * 1 = 1 * 0
    a * (b * c) = (a * b) * c
        where a,b,c::Numbers
-} 
-- one instance per combination 
--      => two different types for numbers!!!
-- of type  Sum
--               and type class  Monoid 
newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  a `mappend` b = Sum (unSum a + unSum b)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  a `mappend` b = Product (unProduct a * unProduct b)

unSum                   :: Sum a -> a
unSum (Sum a)           = a
unProduct               :: Product a -> a
unProduct (Product a)   = a

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================
{-
instance Functor [] where
  fmap = map
instance Foldable [] where
  fold = foldr (mappend) mempty

f == container
       data structure : eles of type : m
       m form a Monoid
          => exist : folding all eles -> ele : m

=>  Foldable : type class
fmap (\s -> mappend s (Sum 2)) (construct (Sum 2)) 
-}

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap       :: Monoid m => (a -> m) -> (f a -> m)
  -- Add a default implementation
  foldMap h xs  = fold (fmap h xs)
 
{-
steps:
(i) fold ( (Sum 1) :> map (flip (:>) []) [Sum 2, Sum 3] )
(ii) using leaves impl
leaves (x:>xs)  = case xs of
                        []          -> 1
                        otherwise   -> foldl (+) 0 (map (\z -> leaves z) xs)  

-} 
instance Foldable Rose where
  fold (x:>xs) = x `mappend` foldr (mappend) mempty (fmap (\z -> fold z) xs)
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- Monoid m => a -> m
--  \x -> Sum x
--  \x -> Product x
-- foldMap 
--  (i) transforms all the elements of the foldable into a Monoid
--  (i) and then folds them into a single monoidal value.
-- fold and fmap
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum xs     = unSum (foldMap Sum xs)
fproduct xs = unProduct (foldMap Product xs)

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

