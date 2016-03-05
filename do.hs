talk1 = do
    putStrLn "What's up? "
    l <- getLine
    putStrLn $ "Got it: " ++ l ++ "!"

talk = putStrLn "What? " >> getLine >>= putStr . ((++) "Got it: ") >> putStrLn "!"
