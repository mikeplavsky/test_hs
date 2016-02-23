data Result a = Data a | Error (a, String) deriving Show

getIt (-1) = Error(-1, "too negative")
getIt a = Data a

instance Monad Result where
    (>>=) (Data a) f = f a
