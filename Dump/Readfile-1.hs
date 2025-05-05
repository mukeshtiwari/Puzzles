import System.IO 
import Data.Char 
import Data.List
import Text.Printf
import Control.Monad
import Network.Pcap
import Foreign.Ptr 
import Data.Word
import qualified Data.ByteString.Char8 as BS 
import Control.Monad

callBack :: PktHdr -> BS.ByteString -> IO ()
callBack p q = do
	--print $ hdrSeconds p 
	--print $ hdrUseconds p
        --print $ hdrCaptureLength p
        --print $ hdrWireLength p
	let ( a , b ) = BS.breakSubstring ( BS.pack "B6034" ) q 
	if ( not.BS.null $ b ) then
	   do
		--print b 
		let ( issue_code , r_1 ) = BS.splitAt 12 . BS.drop 5 $ b 	
		print issue_code
		let ( bid_qtpr , r_2 ) = BS.splitAt 60 . BS.drop 12 $ r_1 
		print bid_qtpr
		let ( ask_qtpr , r_3 ) = BS.splitAt 60 . BS.drop 7 $ r_2 
		print ask_qtpr
		let ( qtacceptm , _ ) = BS.splitAt 8 . BS.drop 50 $ r_3 
		print qtacceptm 
	  else 
		return ()
	
main = openOffline "network.pcap" >>= \x -> loopBS x ( -1 )  callBack   
