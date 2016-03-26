data Tree a = Leaf a | Branch (Tree a) (Tree a) 
    deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)   
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)  
