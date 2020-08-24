import System.IO
import System.Directory
import Data.List

main :: IO ()
main = do
    contents <- readFile "./src/todo/todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [1..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "./src/todo/todo.txt"
    renameFile tempName "./src/todo/todo.txt"