import System.IO
import Data.Char

main :: IO ()
main = do
    contents <- readFile "./src/io/baabaa.txt"
    writeFile "./src/io/baabaacaps.txt" $ map toUpper contents