module Game where 

import System.Random
import Data.Char (toLower)
import Data.Foldable (traverse_)
import System.Console.ANSI

import Type

-- Store all the words that may came out in the game 
dictionary :: [String]
dictionary = ["hello", "earth", "paint", "about", "brain", "chain", "pause", "trade", "burst"]

-- Function to randomly pick a word
selectRandomWord :: IO String
selectRandomWord = randomRIO (0, length dictionary - 1) >>= \index -> return (dictionary !! index)

playGame :: (GameStateOps s) => s -> IO GameResult
playGame state
    | getRemainingAttempts state == 0 = do
        putStrLn $ "You failed to guess the word. The correct word was: " <> getTargetWord state
        return Lose
    | otherwise = do
        putStrLn $ "Enter your guess (" <> show (getRemainingAttempts state) <> " attempts left):"
        guess <- getLine
        handleGuess state (map toLower guess)

askReplay :: IO Int
askReplay = do
    
    replay <- getLine
    if replay == "1" then return 1 else replay == _ return 0 

handleGuess :: (GameStateOps s) => s -> String -> IO GameResult
handleGuess state guess
    | not (isValidGuess guess) = do
        putStrLn "Invalid input. Please enter a valid 5-letter word."
        playGame state
    | guess == getTargetWord state = do
        let score = calculateScore state
        putStrLn $ "Congratulations! Your score is: " <> show score
        return $ Win score
    | otherwise = do
        let feedback = checkAnswer guess (getTargetWord state)
        putStrLn "Feedback on your guess:"
        traverse_ showFeedback feedback
        playGame $ decrementAttempts state

isValidGuess :: String -> Bool
isValidGuess guess = all (`elem` ['a'..'z']) guess && length guess == 5

calculateScore :: (GameStateOps s) => s -> Score
calculateScore state = Score $ getRemainingAttempts state

-- Function to check validity of user input and handle errors
getValidInput :: (Int -> Bool) -> IO Int
getValidInput isValid =
    getLine >>= \input ->
        case reads input :: [(Int, String)] of -- separate into tuple of (Int, String)
            [(n, "")]
                | isValid n -> return n -- return the number only when String is empty
                | otherwise -> retry -- else prompt user to enter again
            _ -> retry
  where
    retry = putStrLn "Invalid input. Please try again." >> getValidInput isValid


-- Function to label which character is correct, misplaced, or incorrect
labelAnswer :: Char -> String -> Char -> Feedback
labelAnswer user letter target
  | user == target = Correct         -- Correct letter in the correct position
  | user `elem` letter = Misplaced  -- Correct letter but wrong position
  | otherwise = Incorrect          -- Incorrect letter

-- Function to check the answer for the entire guess
checkAnswer :: String -> String -> [(Char, Feedback)]
checkAnswer guess target =
    let correctPass = zipWith (\g t -> if g == t then (g, Correct) else (g, Incorrect)) guess target
        misplacedPass = fmap (\(g, feedback) -> if feedback == Incorrect && g `elem` target then (g, Misplaced) else (g, feedback)) correctPass
    in misplacedPass

-- Function to show the labeled answer with colours
showFeedback :: (Char, Feedback) -> IO ()
showFeedback (char, Correct) =
    setSGR [SetColor Foreground Vivid Green] >> -- Green for correct letters
    putStrLn (char : " is Correct") >>
    setSGR [Reset]

showFeedback (char, Misplaced) =
    setSGR [SetColor Foreground Vivid Yellow] >> -- Yellow for misplaced letters
    putStrLn (char : " is Misplaced") >>
    setSGR [Reset]

showFeedback (char, Incorrect) =
    setSGR [SetColor Foreground Vivid Red] >> -- Red for incorrect letters
    putStrLn (char : " is Incorrect") >>
    setSGR [Reset]
