import System.ZMQ3
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Internal as BL
import Data.IORef
import Control.Concurrent ( threadDelay )
main = do 
     c <- context
     s <- socket c Rep
     bind s "tcp://127.0.0.1:5555"
     counter <- newIORef 0 
     forever $ do 
             t <- readIORef counter
             res <- receive s
             print res
             send' s [] ( BL.packChars $ "I got you. " ++ show t )
             modifyIORef counter ( +1 ) 
             threadDelay 10000
     close s
     destroy c
     return () 


