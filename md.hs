data Result a = Data a | Error (a, String) deriving Show

getIt (-1) = Error(-1, "too negative")
getIt a = Data a

instance Functor Result where
    fmap f (Data a) = Data (f a) 

instance Applicative Result where
    pure = Data
    (<*>) (Data f) (Data a) = Data (f a)

instance Monad Result where
    (>>=) (Data a) f = f a
