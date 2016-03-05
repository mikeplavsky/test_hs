f x | x < 5 = x - 1 
    | x >= 5 = x + 1

prop_f x | x < 5 = f (f x) < x
         | x >= 5 = f (f x) > x
