import Control.Parallel.Strategies

get x = 
    let fib = 1:1:[x+y | (x,y) <- zip fib (tail fib)]
    in fib !! x

parMap' f xs = map f xs `using` parList rseq
parMap'' f xs = map f xs `using` parList rpar
