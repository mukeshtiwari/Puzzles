import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified DigitHoles (numHoles)

getVars :: TC.Parser (Int)
getVars = do number <- TC.spaces >> TC.parseInt
             return (number)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (number) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ DigitHoles.numHoles number
            hClose hOut
    hClose hIn
