data Tree a = Leaf a | Branch (Tree a) (Tree a) 
    deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)   
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)  
    
number (Leaf a) s = (Leaf s, s + 1)
number (Branch l r) s = 
    let (l',s') = number l s
        (r',s'') = number r s'
    in (Branch l' r', s'')    

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
