-- Define a GameState typeclass
import System.Random
import Data.Char (toLower)
import Data.Foldable (traverse_)
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

main :: IO ()
main = getUserChoice >>= processUserInput