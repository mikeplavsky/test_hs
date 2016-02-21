import System.Environment

data Next a = Super a | Normal a deriving Show

instance Functor Next where
   fmap f (Super a) = Super (f a)
   fmap f (Normal a) = Super (f a)

instance Applicative Next where
    pure = Super
    Super f <*> m = fmap f m

instance Monad Next where 
    (>>=) (Super a) f = f a

fib_s = 1 : 1 : [a + b | (a,b) <- zip fib_s $ tail fib_s]

getH = do
    x <- getEnvironment
    return [b | (a,b) <- x, a == "HOSTNAME"]

lucky :: (Integral a) => a -> String
lucky 7 = "Wow!"
lucky x = "No!"

fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fib' x 
    | x == zero = one
    | x == one = one
    | otherwise = fib'(x-one) + fib'(x-two)
    where zero = 0
          one = 1
          two = 2 

maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

qs [] = []
qs (x:xs) = 
    let smS = qs [a | a <- xs, a <= x]
        bS = qs [a | a <- xs, a > x]
    in smS ++ [x] ++ bS    
