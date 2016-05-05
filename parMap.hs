import Control.Parallel.Strategies
import System.Environment

parMap' f [] = return []

parMap' f (a:as) = do
    
    b <- rpar $ f a
    bs <- parMap' f as

    return (b:bs)

fib = 1:1:[x+y | (x,y) <- zip fib (tail fib)] 

main' f = do

    file <- readFile f

    let ds = map read $ lines file  :: [Int]
    let w = runEval (parMap' ((!!) fib) ds) 

    return w

main = do

    [f] <- getArgs   
    r <- main' f

    print r


