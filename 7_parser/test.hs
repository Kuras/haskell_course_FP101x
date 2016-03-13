module Parsing where

import Data.Char
import Control.Monad

type Parser a = String -> [(a, String)]

-- basic
item :: Parser Char
item = \ inp -> case inp of
                []      -> []
                (x:xs)  -> [(x,xs)]
                
-- always fail
failure :: Parser a
failure = \ inp -> []

-- always succed
return :: a -> Parser a
return v = \ inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = \inp -> case p inp of
                    []          -> parse q inp
                    [(v,out)]   -> [(v,out)]   
-- succed with singleton list "[(v,out)]" of the value v and Rest of inp 
                    
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

-- first MONAD
-- p :: Parser (Char, Char)
p  = do x <- item
        item
        y <- item
        Parsing.return (x,y)
           