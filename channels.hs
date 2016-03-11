import Control.Concurrent
import Control.Concurrent.Chan

reader ch = do

    res <- readChan ch
    print $ "Got: " ++ res ++ "!"
    
    reader ch
    print "Done."


main = do 

   ch <- newChan
   writeChan ch "Checking..."

   forkIO $ reader ch
