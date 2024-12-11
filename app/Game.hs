module Game where 

-- import third-party library
import System.Random ( randomRIO )
import Data.Char (toLower)
import Data.Foldable (traverse_)
import System.Console.ANSI
    ( SGR(Reset, SetColor),
      ConsoleLayer(Foreground),
      ColorIntensity(Vivid),
      Color(Red, Green, Yellow),
      setSGR )

-- import other modules 
import Type
    ( GameStateOps(getRemainingAttempts, getTargetWord,
                   decrementAttempts),
      GameResult(..),
      Score(..),
      Feedback(..) )

-- Store all the words that may came out in the game 
dictionary :: [String]
dictionary = ["about", "alarm","brain", "black","chain", "chess", 
              "drunk", "dream", "earth", "eagle", "false", "fairy",
              "grass", "green", "happy", "heart","input", "index",
              "jewel", "juice", "knife", "lemmon", "light", "march", "metal",
              "order", "offer", "paint", "queue", "royal", "rural", "seven", "sharp", "today", "touch",
              "under", "upper", "wrong", "write", "young", "voice", "white"]

-- Function to randomly pick a word
selectRandomWord :: IO String
selectRandomWord = randomRIO (0, length dictionary - 1) >>= \index -> return (dictionary !! index)

-- function to handle game logic 
playGame :: (GameStateOps s) => s -> IO GameResult
playGame state
    -- display error message and end the game if used up all attempts
    | getRemainingAttempts state == 0 = 
        putStrLn ("You failed to guess the word. The correct word was: " <> getTargetWord state) >> return Lose

    -- show feedback message and decrement attempt by 1 if still got remaining attempts
    | otherwise = 
        putStrLn ("Enter your 5 letters guess (" <> show (getRemainingAttempts state) <> " attempts left):") >>
        (fmap (map toLower) getLine >>= \guess -> handleGuess state guess)

-- Function to check user's answer with the choosen random word
handleGuess :: (GameStateOps s) => s -> String -> IO GameResult
handleGuess state guess
    -- Handle error when user did not enter all letters
    | not (isValidGuess guess) = 
        setSGR [SetColor Foreground Vivid Red] *> -- Change color to red
        putStrLn "!!! Invalid input. Please enter a valid 5-letter word." *>
        setSGR [Reset] *> -- Reset color
        playGame state

    -- If user guess the correct word, then Win and record the score
    | guess == getTargetWord state = do
        let score = calculateScore state
        putStrLn $ "Congratulations! Your score is: " <> show score
        return $ Win score

    -- If user did not guess the correct word, then lose 
    | otherwise = 
        let feedback = checkAnswer guess (getTargetWord state)
        in putStrLn "Feedback on your guess:" *>
           traverse_ showFeedback feedback *>
           playGame (decrementAttempts state)

-- helper function for handleGuess to make sure input is all letters 
isValidGuess :: String -> Bool
isValidGuess guess = all (`elem` ['a'..'z']) guess && length guess == 5

-- helper function for handleGuess to calculate the score based on the remaining attempts
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
    retry = 
        setSGR [SetColor Foreground Vivid Red] >> -- Set text color to red
        putStrLn "!!! Invalid input. Please try again." >>
        setSGR [Reset] >> -- Reset to default color 
        putStrLn "Enter your choice" >>
        getValidInput isValid


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
