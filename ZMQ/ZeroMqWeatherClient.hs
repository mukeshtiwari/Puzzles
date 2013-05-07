import System.ZMQ3
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Control.Concurrent ( threadDelay )
import System.Random

main = do 
     c <- context
     subscriber <- socket c Sub
     connect subscriber "tcp://127.0.0.1:5556"
     subscribe subscriber ( BS.pack "1000" )
     subscribe subscriber ( BS.pack "1010" )
     forever $ do
             update <- receive subscriber
             print update

     close subscriber
     destroy c       
             