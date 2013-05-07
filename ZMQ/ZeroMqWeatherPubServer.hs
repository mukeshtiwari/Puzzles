import System.ZMQ3
import Control.Monad
import qualified Data.ByteString.Char8 as BS hiding ( putStrLn )
import qualified Data.ByteString.Lazy.Internal as BL
import Control.Concurrent ( threadDelay )
import System.Random

main = do 
     c <- context
     publisher <- socket c Pub
     bind publisher "tcp://127.0.0.1:5556"
     bind publisher "ipc://weather.ipc"
     forever $ do 
             zipcode <- randomRIO ( ( 500 , 2000 ) ::  ( Int , Int ) )
             temp <- randomRIO ( 10 , 45 ) :: IO Int 
             relhum <- randomRIO ( 0 , 100 ) :: IO Int
             putStrLn $ show zipcode ++ " " ++ show temp ++ " " ++ show relhum
             send' publisher [] ( BL.packChars $ show zipcode ++ " " ++ show temp ++ 
                                " " ++  show relhum )
     close publisher
     destroy c
     return ()
