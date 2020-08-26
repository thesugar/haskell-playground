import System.IO

main :: IO ()
main = do
    putStrLn "openFile 関数を使った処理を試す場合は 1 を、withFile 関数を使った処理を試す場合は 2 を、readFile を使った処理を試す場合は 3 を入力してください"
    l <- getLine
    if (l == "1") then
        main1
    else if (l == "2") then
        main2
    else if (l == "3") then
        main3
    else do
        putStrLn "もう一度お試しください。"
        main

main1 :: IO ()
main1 = do
    putStrLn "openFile を使ってハンドルを取得し、hClose を使ってハンドルを閉じるかたちで処理します。"
    handle <- openFile "./src/io/baabaa.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main2 :: IO ()
main2 = do
    putStrLn "withFile を使って処理します。"
    withFile "./src/io/baabaa.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents

main3 :: IO ()
main3 = do
    putStrLn "readFile を使って処理します。"
    contents <- readFile "./src/io/baabaa.txt"
    putStr contents