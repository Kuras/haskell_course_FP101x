
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