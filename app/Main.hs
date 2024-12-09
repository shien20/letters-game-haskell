-- Define a GameState typeclass
import System.Random
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Sum(..))
import Data.Foldable (traverse_)
import Text.Printf (printf)
import System.Console.ANSI


class GameStateOps s where
    decrementAttempts :: s -> s             -- Reduce the remaining attempts
    setTargetWord     :: String -> s -> s   -- Set the target word
    getRemainingAttempts :: s -> Int        -- Get the number of remaining attempts
    getTargetWord     :: s -> String        -- Get the target word

-- Implement GameStateOps for the GameState data type
instance GameStateOps (GameState a) where
    decrementAttempts (GameState attempts target val) =
        GameState (attempts - 1) target val

    setTargetWord word (GameState attempts _ val) =
        GameState attempts word val

    getRemainingAttempts (GameState attempts _ _) = attempts

    getTargetWord (GameState _ target _) = target


-- | Newtype for Score
newtype Score = Score Int deriving (Show, Eq, Ord)

instance Semigroup Score where
    (Score a) <> (Score b) = Score (a + b)

instance Monoid Score where
    mempty = Score 0
    mappend = (<>)


-- | GameState encapsulates the game data
data GameState a = GameState
    { remainingAttempts :: Int
    , targetWord :: String
    , stateValue :: a
    } deriving Show

-- Functor, Applicative, and Monad instances for GameState
instance Functor GameState where
    fmap f (GameState attempts target val) = GameState attempts target (f val)

instance Applicative GameState where
    pure x = GameState 6 "" x
    (GameState _ _ f) <*> (GameState attempts target val) =
        GameState attempts target (f val)

instance Monad GameState where
    return = pure
    (GameState attempts target val) >>= f =
        let (GameState newAttempts newTarget newVal) = f val
         in GameState (min attempts newAttempts) (if null target then newTarget else target) newVal

-- Custom type for results
data GameResult = Lose | Win Score deriving (Show, Eq)
-- Feedback data type to label whether a letter is correct, misplaced or incorrect
data Feedback = Correct | Misplaced | Incorrect deriving (Show, Eq)

-- ---------------------------------------------- MENU -------------------------------------------------------------
-- | Menu
getUserChoice :: IO Int
getUserChoice =
    putStrLn "\n--- LETTERS GAME ---" >>
    putStrLn "[1] Start Game" >>
    putStrLn "[2] View History" >>
    putStrLn "[3] View Instructions" >>
    putStrLn "[4] Generate Report" >>
    putStrLn "[5] Exit" >>
    getValidInput "Enter your choice: " (\n -> n >= 1 && n <= 5)


getValidInput :: String -> (Int -> Bool) -> IO Int
getValidInput prompt isValid =
    putStr prompt >>
    getLine >>= \input ->
        case reads input :: [(Int, String)] of
            [(n, "")]
                | isValid n -> return n
                | otherwise -> retry
            _ -> retry
  where
    retry = putStrLn "Invalid input. Please try again." >> getValidInput prompt isValid



processUserInput :: Int -> IO ()
processUserInput choice =
    putStrLn "-----------------------------------------------------------------------" >>
    case choice of
        1 -> startGame
        2 -> viewHistory
        3 -> viewInstructions
        4 -> generateReport 
        5 -> putStrLn "Thank you for playing! Goodbye."
        _ -> putStrLn "Invalid choice. Please try again." >> getUserChoice >>= processUserInput

-- ---------------------------------------------- MENU -------------------------------------------------------------


-- ---------------------------------------------- GAME -------------------------------------------------------------


-- | Dictionary
dictionary :: [String]
dictionary = ["hello", "earth", "paint", "about", "brain", "chain", "pause", "trade", "burst"]

targetedWord :: IO String
targetedWord = randomRIO (0, length dictionary - 1) >>= \index -> return (dictionary !! index)



-- | Game Logic with Monad
startGame :: IO ()
startGame = do
    target <- targetedWord
    let initialState = GameState 6 target ()
    result <- playGame initialState
    logGameResult "game_records.txt" target result
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "Do you want to play again? \npress [1] to play again OR press [any key] Exit to Menu"
    replay <- getLine
    if replay == "1" then startGame else main 

logGameResult :: FilePath -> String -> GameResult -> IO ()
logGameResult filePath word result =
    let record = case result of
            Win (Score s) -> "Word: " ++ word ++ " | Result: Win | Score: " ++ show s
            Lose -> "Word: " ++ word ++ " | Result: Lose | Score: 0"
    in appendFile filePath (record ++ "\n")

playGame :: (GameStateOps s) => s -> IO GameResult
playGame state
    | getRemainingAttempts state == 0 =
        putStrLn ("You failed to guess the word. The correct word was: " <> getTargetWord state)
        >> return Lose
    | otherwise =
        putStrLn ("Enter your guess (" <> show (getRemainingAttempts state) <> " attempts left):")
        >> getLine
        >>= \guess -> 
            if not (all (`elem` ['a'..'z']) (map toLower guess)) then
                putStrLn "Invalid input. Your guess contains numbers or special characters. Please enter a valid 5-letter word."
                >> playGame state
            else if length guess /= 5 then
                putStrLn "Invalid input. Please enter a 5-letter word."
                >> playGame state
            else if map toLower guess == getTargetWord state then
                let scoreState = (Score <$> pure (getRemainingAttempts state))
                in putStrLn ("Congratulations you got the answer! Your score is: " <> show (stateValue scoreState))
                >> return (Win (stateValue scoreState))
            else
                let feedback = checkAnswer (map toLower guess) (getTargetWord state) in
                putStrLn "Feedback on your guess:"
                >> traverse_ showFeedback feedback
                >> playGame (decrementAttempts state)





-- Function to label which character is correct, misplaced, or incorrect
labelAnswer :: Char -> String -> Char -> Feedback
labelAnswer user target comp
  | user == comp = Correct         -- Correct letter in the correct position
  | user `elem` target = Misplaced  -- Correct letter but wrong position
  | otherwise = Incorrect          -- Incorrect letter

-- Function to check the answer for the entire guess
checkAnswer :: String -> String -> [(Char, Feedback)]
checkAnswer guess target =
    let correctPass = zipWith (\g t -> if g == t then (g, Correct) else (g, Incorrect)) guess target
        misplacedPass = fmap (\(g, feedback) -> if feedback == Incorrect && g `elem` target then (g, Misplaced) else (g, feedback)) correctPass
    in misplacedPass


-- Function to display the feedback in a readable format with colors
showFeedback :: (Char, Feedback) -> IO ()
showFeedback (char, Correct) = do
    -- Set the color to green for Correct feedback
    setSGR [SetColor Foreground Vivid Green]
    putStrLn (char : " is Correct")
    -- Reset to default colors
    setSGR [Reset]

showFeedback (char, Misplaced) = do
    -- Set the color to yellow for Misplaced feedback
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn (char : " is Misplaced")
    -- Reset to default colors
    setSGR [Reset]

showFeedback (char, Incorrect) = do
    -- Set the color to red for Incorrect feedback
    setSGR [SetColor Foreground Vivid Red]
    putStrLn (char : " is Incorrect")
    -- Reset to default colors
    setSGR [Reset]

-- ---------------------------------------------- GAME -------------------------------------------------------------

-- ---------------------------------------------- INSTRUCTIONS -------------------------------------------------------------
-- | View Instructions
viewInstructions :: IO ()
viewInstructions =
    putStrLn "HOW TO PLAY THE LETTERS GAME" >>
    putStrLn "1. Guess the 5-letter word in 6 attempts." >>
    putStrLn "2. Feedback is provided for each letter." >>
    putStrLn "3. Score decreases with more attempts.\n" >>
    putStrLn "Which operation would you like to perform: " >>
    putStrLn "[1] Start Game [2] Exit to Menu" >>
    getValidInput "Enter your choice: " (\n -> n >= 1 && n <= 2) >>= \choice ->
        case choice of
            1 -> startGame
            2 -> do 
                choice <- getUserChoice
                processUserInput choice 


-- ---------------------------------------------- INSTRUCTIONS -------------------------------------------------------------


-- ---------------------------------------------- HISTORY -------------------------------------------------------------

-- View History
viewHistory :: IO ()
viewHistory =
    readHistory "game_records.txt"
    >>= \records ->
        if null records
            then putStrLn "No game records found." >> main 
            else putStrLn "\nYOUR GAME HSITORY:"
                 >> mapM_ (putStrLn . formatRecord) (zip [1..] records)
                 >> putStrLn "\nWhich operation would you like to perform\n[1] Delete Record [2] Exit to Menu"
                 >> getValidInput "Enter your choice: " (\n -> n >= 1 && n <= 2)
                 >>= \input ->
                    putStrLn "-----------------------------------------------------------------------\n" >>
                    case input of
                        1 -> deleteRecords
                        2 -> main

deleteRecords :: IO ()
deleteRecords =
    readHistory "game_records.txt"
    >>= \records ->
        if null records
            then putStrLn "No game records found to delete." >> main 
            else do
                putStrLn "DELETING YOUR GAME HISTORY\n"
                traverse_ (putStrLn . formatRecord) (zip [1..] records)
                putStrLn "Enter the record number to delete or 0 to cancel:"
                getLine >>= \input ->
                    case validateDeletion input records of
                        Left errMsg -> putStrLn errMsg >> viewHistory
                        Right updatedRecords -> writeFile "game_records.txt" (unlines updatedRecords)
                            >> putStrLn "Record deleted successfully.\n-----------------------------------------------------------------------\nYour record has been UPDATED"
                            >> viewHistory

validateDeletion :: String -> [String] -> Either String [String]
validateDeletion input records =
    case reads input :: [(Int, String)] of
        [(n, "")]
            | n == 0 -> Left "No records deleted.\n-----------------------------------------------------------------------"
            | n > 0 && n <= length records -> Right $ deleteRecord n records
            | otherwise -> Left "Invalid record number. Please try again."
        _ -> Left "Invalid input. Please enter a valid record number."







-- Helper to format a record with its index
formatRecord :: (Int, String) -> String
formatRecord (i, record) = show i ++ ". " ++ record

-- Helper to delete a record by its index (1-based)
deleteRecord :: Int -> [String] -> [String]
deleteRecord n = foldr (\(i, line) acc -> if i == n then acc else line : acc) [] . zip [1..]

-- Read History function to read game records (as you already have)
readHistory :: FilePath -> IO [String]
readHistory filePath =
    doesFileExist filePath >>= \exists ->
        if exists
            then lines <$> readFile filePath
            else return []


-- ---------------------------------------------- HISTORY -------------------------------------------------------------

-- ---------------------------------------------- REPORT -------------------------------------------------------------

generateReport :: IO ()
generateReport =
    readHistory "game_records.txt" >>= \records ->
        let totalGames = calculateTotalGames records
            wins = calculateWins records
            losses = calculateLosses totalGames wins
            bestScore = (scoreValue <$> calculateBestScore records)  -- Extract numerical value
            totalScore = calculateTotalScore records
            averageScore = calculateAverageScore (getSum totalScore) totalGames
            insights = generateInsights totalGames averageScore
        in putStrLn "YOUR GAME REPORT: \n-----------------------------------------------------------------------"
           >> putStrLn ("Total Games: " <> show totalGames)
           >> putStrLn ("Wins: " <> show wins <> ", Losses: " <> show losses)
           >> putStrLn ("Best Score: " <> maybe "No scores yet." show bestScore)
           >> printf "Average Score: %.2f\n" averageScore  -- Directly use printf
           >> putStrLn insights
           >> putStrLn "-----------------------------------------------------------------------"

calculateTotalGames :: [String] -> Int
calculateTotalGames = length


calculateWins :: [String] -> Int
calculateWins = length . filter ("Win" `isInfixOf`)

calculateLosses :: Int -> Int -> Int
calculateLosses totalGames wins = totalGames - wins

calculateTotalScore :: [String] -> Sum Int
calculateTotalScore = foldMap (Sum . scoreValue) . mapMaybe parseScore

calculateBestScore :: [String] -> Maybe Score
calculateBestScore = foldr (maxScore . parseScore) Nothing
  where
    maxScore :: Maybe Score -> Maybe Score -> Maybe Score
    maxScore Nothing y = y
    maxScore x Nothing = x
    maxScore (Just a) (Just b) = Just (max a b)

calculateAverageScore :: Int -> Int -> Double
calculateAverageScore totalScore totalGames =
    if totalGames > 0
        then fromIntegral totalScore / fromIntegral totalGames
        else 0

generateInsights :: Int -> Double -> String
generateInsights totalGames averageScore =
    "You scored an average of " <> printf "%.2f" averageScore <>
    " points from a total of " <> show totalGames <> " games."



-- Helper function to get the integer value from Score
scoreValue :: Score -> Int
scoreValue (Score s) = s

parseScore :: String -> Maybe Score
parseScore record
    | "Win" `isInfixOf` record = -- Backticks added here
        let scoreStr = last (words record)
         in Score <$> readMaybe scoreStr
    | otherwise = Nothing


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing


-- ---------------------------------------------- REPORT -------------------------------------------------------------

main :: IO ()
main = getUserChoice >>= processUserInput