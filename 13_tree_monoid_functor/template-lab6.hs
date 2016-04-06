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

tree = 'x' :> map (flip (:>) []) ['a'..'x']
tree' = 'x' :> map (\c -> c :> []) ['a'..'A']

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

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
                        []          -> (f x) :> []
                        otherwise   -> (f x) :> (map (\z -> fmap f z) xs)

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- Exercise 9
-- \Why Rose a -> Rose b
f r = fmap head $ fmap (\x -> [x]) r

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = error "you have to implement mempty for Sum"
  mappend = error "you have to implement mappend for Sum"
  
instance Num a => Monoid (Product a) where
  mempty = error "you have to implement mempty for Product"
  mappend = error "you have to implement mappend for Product"

unSum :: Sum a -> a
unSum = error "you have to implement unSum"
unProduct :: Product a -> a
unProduct = error "you have to implement unProduct"

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap = error "you have to implement foldMap"
  
instance Foldable Rose where
  fold = error "you have to implement fold for Rose"
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum = error "you have to implement fsum"
fproduct = error "you have to implement fproduct"

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

