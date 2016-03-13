{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}

data Term = Con Int | Div Term Term

answer', error' :: Term
answer'         = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
error'          = (Div (Con 1 ) (Con 0 ))

main = do
        print (eval answer')
        print (eval' answer' 0)
        print (eval'' answer')


data M a = Raise Exception | Return a
type Exception = String

instance Show (M Int) where
    show (Raise e) = e
    show (Return a) = show a

instance Show Term where
    show (Con e) = show e
    show (Div t u) = "Div (Con "++ (show t) ++") (Con "++ (show u) ++") /n"

-- EXCEPTIONS --
eval            :: Term -> M Int
eval (Con a)    = Return a
eval (Div t u)  = case eval t of
                        Raise e -> Raise e
                        Return a ->
                            case eval u of
                                Raise e -> Raise e
                                Return b ->
                                    if b == 0
                                        then Raise "dividing by zero moron"
                                        else Return (a `div` b)


-- STATE --
--- count number of divisions
--- impure  => state
----    count = 0
----    count ++
--- pure    => monads -> introducing new TYPE
---             to represent computations that act on state

type M' a = State -> (a,State)
type State = Int

-- Type is M'a! : value: function

eval'               :: Term -> M' Int
eval' (Con a) x     = (a,x)
eval' (Div t u) x   = let (a,y) = eval' t x in
                      let (b,z) = eval' u y in
                      (a`div`b, z + 1)

-- eval' (Div (Div (Con 1972) (Con 2)) (Con 23)) 0
--      (1972/2,1) <- eval' (Div (Con 1972) (Con 2)) 0
--          (1972,0) <- eval' (Con 1972) 0
--          (2,0) <- eval' (Con 2) 0
--          (1972/2, 0 + 1)
--  (23,1) <- eval' (Con 23) 1
--  (1972/2/23, 1 + 1)
--  (42, 2)

    
-- eval' (Con 2) 0 = (2,0)
--  ({2} State -> (a,State)) 0
--  (State -> (2,State)) 0
--  0 -> (2,0)      



-- OUTPUT --
-- trace of execution
-- impure -> inserting output commands
-- pure -> introducting new TYPE and apply to evaluator

type M'' a      = (Output,a)
type Output     = String

eval''          :: Term -> M'' Int
eval''(Con a)       = (line (Con a) a,a)
eval''(Div t u)     = let (x,a) = eval'' t in
                      let (y,b) = eval'' u in
                      (x ++ y ++ line (Div t u)(a`div`b), a`div`b)

line            :: Term -> Int -> Output
line t a        = "eval''(" ++ (show t) ++ ") <= " ++ (show a) ++ "/n"

-- eval'' (Con 2)
-- eval :: Term -> Int
-- eval (Con a) = a
-- eval (Div t u) = eval t / eval u

--- All impure stuff could be mimicked by monads ---


-- Each of the variations has a similar structure
--      eval            :: Term -> M Int
--      eval            :: Term -> M' Int
--      eval            :: Term -> M'' Int
--      eval            :: a    -> M b
--  => abstracted
--        => to yield : monad
--        => flexilble
--  => variations.each
--          type of computations
--
--  => a -> b ===> a -> M b : monad representation
--           method a -> b
--              + effect in M
--                  : state
--                  : stack trace
--                  : exception

---- Fast and Loose ----
--    => reasoning