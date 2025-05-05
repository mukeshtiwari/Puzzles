import Network.HTTP
import Text.HTML.TagSoup


main = do 
	x <- getLine 
	htmlpage <-  getResponseBody =<< simpleHTTP ( getRequest x ) --open url
	writeFile "/home/user/Programs/Mukesh_Tiwari/Haskell/Download/down.html" htmlpage
	
