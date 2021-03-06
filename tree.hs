import Control.Monad

data Tree a = Leaf a | Branch (Tree a) (Tree a) 
    deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)   
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)  
    
-- number part

newtype State' s a = State' {runState :: s -> (a,s)}

-- return'' :: a -> State' s a
return'' a = \s -> (a,s)

type State s a = s -> (a,s)

return' :: a -> State s a
return' a = \s -> (a,s)

(>>>=>>>) :: State s a -> (a -> State s b) -> State s b
(>>>=>>>) x f = \s -> let (a,s') = x s in f a s'

number (Branch l r) = 
    number l >>>=>>> \l' ->
    number r >>>=>>> \r' ->
    return' (Branch l' r')

number (Leaf a) = tick >>>=>>> \s ->
    return' (Leaf s)

tick s = (s, s + 1)

-- zip part

zipTree (Leaf a) (Leaf b) = return $ Leaf (a, b)

zipTree (Branch l r) (Branch l' r') = do
    liftM2 Branch (zipTree l l') (zipTree r r')

zipTree _ _ = Nothing
