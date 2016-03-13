factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
  where a = 10
        xs = [1..5]
        
double n = n*2
