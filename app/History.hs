module History where

import System.Directory (doesFileExist)

import Type ( GameResult(..), Score(Score) )

-- Read History function to read game records
readHistory :: FilePath -> IO [String]
readHistory filePath =
    doesFileExist filePath >>= \exists -> -- check if file path exist
        if exists
            then lines <$> readFile filePath -- if exist, read the content into a list 
            else return [] -- if does not exist, return an empty list

-- Handle error when deleting file out of index
validateDeletion :: String -> [String] -> Either String [String]
validateDeletion input records =
    case reads input :: [(Int, String)] of 
        [(n, "")] -- ensure user entered numbers only
            | n == 0 -> Left "No records deleted.\n-----------------------------------------------------------------------" -- exit delete program if user choose [0] Cancel delete
            | n > 0 && n <= length records -> Right $ deleteRecord n records -- if user entered index within range, perform deletion
            | otherwise -> Left "Invalid record number. Please try again." -- if user entered index out of range, display error message 
        _ -> Left "Invalid input. Please enter a valid record number." -- display error message if user entered text / numbers and text

-- Handle error when file is empty
processHistory :: FilePath -> IO () -> IO [String]
processHistory filePath onEmpty = 
    readHistory filePath >>= \records -> -- Read the contents of file
        if null records
            then putStrLn "No game records found.\nStart playing to create game records" >> onEmpty >> return [] -- If file empty, display a message and return to main menu
            else return records -- If not empty, return the records


-- Helper to format a record with its index
formatRecord :: (Int, String) -> String
formatRecord (i, record) = show i ++ ". " ++ record -- format the record by prepending the index with a "." ("1. records")

-- Remove a record from history by index
deleteRecord :: Int -> [String] -> [String]
deleteRecord n = foldr (\(i, line) x -> if i == n then x else line : x) [] . zip [1..] -- if the index matches the given index, skip the line 

-- Format and display the game records from the file
logGameResult :: FilePath -> String -> GameResult -> IO ()
logGameResult filePath word result =
    let record = case result of
            Win (Score s) -> "Word: " ++ word ++ " | Result: Win | Score: " ++ show s -- if result is "Win", extract and display the Score
            Lose -> "Word: " ++ word ++ " | Result: Lose | Score: 0" -- if result is "Lose", print Score: 0
    in appendFile filePath (record ++ "\n") -- add a newline at the end
