{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype AInt = A {aint::Int} deriving (Show, Eq, Num)

instance Monoid AInt where
    mempty = 0
    mappend = (+)

newtype BInt = B {bint::Int} deriving (Show, Eq, Num)

instance Monoid BInt where
    mempty = 1
    mappend = (*)
