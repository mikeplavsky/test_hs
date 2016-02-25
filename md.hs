data Result a = Data a | Error (Int, String) deriving Show

getIt a 
    | a < 0 = Error(0,show a ++ " too negative")
    | otherwise = Data a

instance Functor Result where
    fmap f (Data a) = Data (f a) 

instance Applicative Result where
    pure = Data
    Data f <*> Data a = Data (f a)

instance Monad Result where
    Data a >>= f = f a
    Error (i,s) >>= _ = Error (i+1,s) 
