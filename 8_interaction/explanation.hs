    parse (char 'a')  "abc"

    -- eval means evaluations function --

    -- because in haskell we have one argument function, so we have trace --
    i)      eval -> parse (char 'a')
    ii)     eval -> parse (sat (== 'a'))
    iii)    eval -> parse (do x <- item
                                if (== 'a') x 
                                    then return x 
                                    else failure)
    -- now return the function which take second argument --
    iv)     eval -> (do x <- item
                                if (== 'a') x 
                                    then return x 
                                    else failure) "abc" 

    -- and now i do not get it --
    -- item return [(x',x's)] --
    -- but in x we have only x', but not [(x',x's)] ! Why is that !--                              



> expr'                         = expr' - nat
>                                    next   <- (do symbol "-"
>                                                    natural)
>                                    return (next)

{
foldl (-) 0 [1,2,3]
(((0-1)-2)-3)

parse expr "0 -1 -2 -3"
[(-6,"")]


}


foldr (-) 0 [1,2,3]
(3-(2-(1-0)))