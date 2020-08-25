import Data.Char
import Control.Monad

main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l