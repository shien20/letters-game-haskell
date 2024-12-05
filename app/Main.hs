module Main where

getUserChoice :: IO Int
getUserChoice = do
    putStrLn "\n--- LETTERS GAME ---"
    putStrLn "[1] Start Game"
    putStrLn "[2] View History"
    putStrLn "[3] View Instructions"
    putStrLn "[4] Generate Report"
    putStrLn "[5] Exit"
    putStr "Enter your choice: "
    getValidInput "Enter your choice: " (\n -> n >= 1 && n <= 5)

getValidInput :: String -> (Int -> Bool) -> IO Int
getValidInput prompt isValid = do
    putStr prompt
    input <- getLine
    case reads input :: [(Int, String)] of
        [(n, "")]
            | isValid n -> return n
            | otherwise -> retry
        _ -> retry
  where
    retry = do
        putStrLn "Invalid input. Please try again."
        getValidInput prompt isValid


processUserInput :: Int -> IO ()
processUserInput choice = case choice of
    1 -> putStrLn "start game"
    2 -> putStrLn "view history"
    3 -> putStrLn "view instructions"
    4 -> putStrLn "generate report" 
    5 -> putStrLn "Thank you for playing! Goodbye."
    _ -> do
        putStrLn "Invalid choice. Please try again."
        newChoice <- getUserChoice
        processUserInput newChoice

main :: IO ()
main = do 
    choice <- getUserChoice
    processUserInput choice