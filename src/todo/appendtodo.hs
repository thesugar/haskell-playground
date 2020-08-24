import System.IO

main :: IO ()
main = do
    todoItem <- getLine
    appendFile "./src/todo/todo.txt" (todoItem ++ "\n")