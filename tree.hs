data Tree a = Leaf a | Branch (Tree a) (Tree a) 
    deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)   
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)  
    
-- number part

return'' x = \s -> (x,s)
(>>>=>>>) x f = \s -> let (a,s') = x s in f a s'

number' (Branch l r) = 
    number l >>>=>>> \l' ->
    number r >>>=>>> \r' ->
    return'' (Branch l' r')

number' (Leaf a) = tick >>>=>>> \s ->
    return'' (Leaf s)

tick s = (s, s + 1)

number (Leaf a) s = (Leaf s, s + 1)
number (Branch l r) s = 
    let (l',s') = number l s
        (r',s'') = number r s'
    in (Branch l' r', s'')    

-- zip part

return' x = Just x
(>>=>>) x f = 
    case x of 
        Nothing -> Nothing
        Just a -> f a

zipTree (Leaf a) (Leaf b) = return' $ Leaf (a, b)

zipTree (Branch l r) (Branch l' r') = 
    zipTree l l' >>=>> \l'' ->  
    zipTree r r' >>=>> \r'' -> 
    return' $ Branch l'' r''

zipTree _ _ = Nothing