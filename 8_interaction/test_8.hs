 -- # OPTIONS_GHC -w #

-- haskell internall DLS --
-- compiling language1 -> language2
-- parsing

-- Interactive Programs
--      higher-order function
--      imperative functions

-- readLine() -> string1
-- readLine() -> string2
-- string1 /= string2
-- readLine() not mathematial fun => not pure
-- side effects => interact
--      new type "IO a"
--                  return value of type a
--                  IO Char
--                  IO ()
--                      perform action with side efect, and retrun empty!
--                      imperative language : void == IO ()
--                      imperative language : unit -> === IO in haskell
--                      imperative === do
--      getChar :: IO Char
--          method with sideffect and return Char
-- language idioms:
--              sea of impurity code and
--                      island of purity

-- top down desing

hangman :: IO ()
hangman =
    do putStrLn "Think of a word: "
       word <- sgetLine
       putStrLn "Try to guess it: "
       guess word

sgetLine       :: IO String
sgetLine       = do x <- getCh
                    if x == '\n' then
                        do putChar x
                           return []
                     else
                        do putChar '-'
                           xs <- sgetLine
                           return (x:xs)
getCh       :: IO Char
-- getCh       =  do hSetEcho stdin False
--                   c <- getChar
--                   hSetEcho stdin True
--                   return c

getCh       =  getChar

guess       :: String -> IO ()
guess word  =
    do putStr "> "
       xs <- getLine
       if xs == word then
          putStrLn "You got it!"
        else
            do putStrLn (diff word xs)
               guess word
-- guess word => tail recursion
-- getChar :: IO Char
-- bind to 'c'
-- c <- getChar
-- <- => get value from computation and bind!!!

-- now we entrance to pure word
diff        :: String -> String -> String
diff xs ys =
   [if x `elem` ys then x else '-' | x <- xs]

main :: IO ()
main = hangman

-- Kotlin --
{-
functional programming
   higer order function
   statement vs. expresion
   doesnt have lazy on list!!!
      => define tail optimalization!!
-}

-- homework --
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStr' "\n"


getLine' :: IO String
getLine'    = get []

get        :: String -> IO String
get xs
    = do x <- getChar
         case x of
             '\n'   -> return xs
             _      -> get (xs ++ [x])
             
             
             
interact' :: (String -> String) -> IO ()
interact' f 
    = do inp <- getLine'
         putStrLn' (f inp)
         
-- infiniti bug  putStrLn'    exe 4   
-- interact' (\x-> x++"kon")

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

-- use
{-
sequence_' []
sequence_' [putStrLn' "jak"]
sequence_' [putStrLn' "Jak masz na imię", interact' (\x-> x++" Koń")]
-}
sequence_'' :: Monad m => [m a] -> m ()
sequence_'' [] = return ()
sequence_'' (m:ms) = m >> sequence_'' ms

sequence_''' :: Monad m => [m a] -> m ()
sequence_''' [] = return ()
sequence_''' (m:ms) = m >>= \ _-> sequence_''' ms

sequence_'''' :: Monad m => [m a] -> m ()
sequence_'''' ms = foldr (>>) (return ()) ms

-------
-- finite, non-partial, list
-- non-bottom, monadic values
-- left to right

-- collecting all (intermediate) results into a list?
sequence1'      :: Monad m => [m a] -> m [a]
sequence1' []   = return []
sequence1' (m:ms) =
    m >>=
      \a ->
        do as <- sequence1' ms
           return (a:as)

sequence5         :: Monad m => [m a] -> m [a]
sequence5      ms = foldr func (return []) ms
     where
          func :: (Monad m) => m a -> m [a] -> m [a]
          func m acc
               = do x <- m
                    xs <- acc
                    return (x:xs)

sequence8         :: Monad m => [m a] -> m [a]
sequence8 []   = return []
sequence8  (m:ms)
     = do a <- m
          as <- sequence8 ms
          return (a:as)

--- Exercise 7 ---


-- non-bottom function
--   a -> m b
-- finite, non-partial list of non-bottom ele
--   [a]

-- produces the resulting list wrapped inside a monadic action

--- preserve order ---
mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1 f as = sequence8 (map f as)

putChar' :: Char -> IO ()
putChar' c = putChar c >> putChar '\n'
-- use
{-
mapM1 (\x -> putChar' x) []
mapM1 (\x -> putChar' x) ['a'..'z']
-}
mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2 f [] = return []
mapM2 f (a:as) 
     = f a >>= \b ->mapM2 f as >>= \bs -> return (b:bs)


mapM6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM6 f [] = return []
mapM6 f (a:as)
     = do 
          b <- f a
          bs <- mapM6 f as
          return (b : bs)

mapM7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM7 f [] = return []
mapM7 f (a:as)
     = f a >>=
          \b ->
               do bs <- mapM7 f as
                  return (b : bs)

---- Exercise 8 ----

-- predicate
--   Monad m => a -> m Bool
-- finite, non-partial list of non-bottom elem
-- order --
filterM2 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM2 _ [] = return []
filterM2 p (x:xs)
     = do flag <- p x
          ys <- filterM2 p xs
          if flag then return (x:ys) else return ys

-- use
{-
filterM1 (\x -> return ((x `mod` 2) == 0)) []
filterM1 (\x -> return ((x `mod` 2) == 0)) [1..10]
-}
---- Exercise 9 ----
-- oo infinite is
-- accumulation function a -> b -> m a

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
{-
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

foldl (-) 0 [1,2,3]
(((0-1)-2)-3)

a =0
b =[1,2,3]
(a -> b -> m a) =(-)
-}
-- The recursive structure of foldLeftM
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = f a x >>= \b -> foldLeftM f b xs

{- use
foldLeftM (\a b -> putChar b >> return (b: a++[b])) [] "haskell" >>= \r -> putStrLn' r
-}

-- Exercise 10 --
-- !oo finite is  !!!!

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
{-
foldr (-) 0 [1,2,3]
(3-(2-(1-0)))

foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
-}
foldRightM f z [] = return z
foldRightM f z (x:xs) = do frm <- foldRightM f z xs
                           f x frm

{- use
foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn' r
-}

-- - Exercise 11 - --
liftM1 :: Monad m => (a -> b) -> m a -> m b
liftM1 f m 
     = do x <- m
          return (f x)

{- use
liftM (id) putStrLn' "12"

-}          

liftM3 :: Monad m => (a -> b) -> m a -> m b
liftM3 f m = m >>= \a -> return (f a)


