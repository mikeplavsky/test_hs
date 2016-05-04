import Control.Parallel.Strategies

parMap' f [] = return []

parMap' f (a:as) = do
    
    b <- rpar $ f a
    bs <- parMap' f as

    return (b:bs)


