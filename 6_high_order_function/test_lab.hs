squares n
    | n == 0 = []
    | n > 0  = squares (n-1) ++ [n^2]



sumSquares n = sum (squares n)

squares' m n = drop n (squares (m+n))

sumSquares' x = sum . uncurry squares' $ (x, x)

coords m n = [(x,y) | x <- [0..m], y <- [0..n]]