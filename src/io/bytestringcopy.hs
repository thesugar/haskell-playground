import System.Environment
import System.IO
import System.Directory
import Control.Exception
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    (fileName1: fileName2: _) <- getArgs
    copy fileName1 fileName2

copy :: String -> String -> IO ()
copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)