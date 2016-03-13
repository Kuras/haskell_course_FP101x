safetail xs = if null xs then [] else tail xs

------------------------4

xs = 1 : [x+1 | x <- xs]  -- recursivly infinite colection

permute :: [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = concatMap (interweave x)(permute xs)

interweave :: a -> [a] -> [[a]]
interweave x [] = [[x]]
interweave x (y:ys) = [x:y:ys] ++ map (y:) (interweave x ys)

-------------interweave---------------
-- interweave 1 [2]
-- = [1:2:[]] ++ map (2:) (interweave 1 [])
-- = [1:2:[]] ++ map (2:) [[1]]
-- = [[1,2]] ++ [[2,1]]
-- map dryluje w głąb, tam gdzie może zastosować - nie 1 poziom

-- [1:2:[]] - 1:(2:[])
-------------concatMap---------------
-- concatMap (\x->[x/2]) [1..5]
-- concatMap (\x->[x*2]) [1..5]

-------------permute---------------
-- permute [1,2] = [[1,2],[2,1]]
-- = concatMap (interweave 1)(permute [2])
-- = concatMap (interweave 2)(permute [])
-- ->
-- (2) concatMap (interweave 2)[[]]
-- (2) [[2]]
-- (1) concatMap (interweave 1)[[2]]
-- (1) [[1,2],[2,1]]
-- power :: (Eq n, Num n, Num a) => a -> n -> a
m `power` 0 = 1
m `power` n = m * m `power` (n-1)

m `power'` 0 = 1
m `power'` n = m * power' m (n-1)
