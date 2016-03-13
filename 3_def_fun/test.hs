-- conditional expresion

abs' n = if n >=0 then n else -n

abs'' n | n >= 0 		= n
		| otherwise 	= -n

-- pattern matching => directly => consise
-- in order -> top => down & left => right

not'         :: Bool -> Bool
not' False = True
not' True  = False

-- in order -> top => down & left => right
and'         :: Bool -> Bool -> Bool
--wrong order matching
_      `and'`   _    = False
True   `and'`   True = True


--and''         :: Bool -> Bool -> Bool
--error gives
-- b `and''` b = False


-- defing using pattern matching
--3:(4:[])
head'       :: [a] -> a
head'   (x:_) = x

-- lambdas
--add x y = x + y
add = \x -> (\y -> x + y)