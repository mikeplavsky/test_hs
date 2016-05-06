import Control.Parallel.Strategies
import System.Environment

parMap' f [] = return []

parMap' f (a:as) = do
    
    b <- rpar $ f a
    bs <- parMap' f as

    return (b:bs)

get x = 
    let fib = 1:1:[x+y | (x,y) <- zip fib (tail fib)]
    in fib !! x

main' f = do

    file <- readFile f

    let ds = map read $ lines file  :: [Int]
    let w = runEval (parMap' get ds) 

    return w

main = do

    [f] <- getArgs   
    r <- main' f

    print r


