import System.ZMQ3
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Data.IORef
import Control.Concurrent ( threadDelay )

main = do 
     c <- context
     s <- socket c Req
     connect s "tcp://127.0.0.1:5555"
     counter <- newIORef 0
     forever $ do 
             t <- readIORef counter
             send' s [] ( BL.packChars $ "Accept hello from Client. " ++ show t )
             msg <- receive s
             print msg
             modifyIORef counter ( +1 ) 
             threadDelay 10000 
     return ()