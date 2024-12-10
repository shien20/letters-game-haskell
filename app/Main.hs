module Main where 

import Data.Semigroup (Sum(..))
import Data.Foldable (traverse_)
import Text.Printf (printf)
import System.Console.ANSI
    ( SGR(Reset, SetConsoleIntensity, SetColor),
      ConsoleIntensity(BoldIntensity),
      ConsoleLayer(Foreground),
      ColorIntensity(Vivid),
      Color(Red, Green, Yellow, Blue, Cyan),
      setSGR )
import Control.Monad (unless)

import Game ( getValidInput, selectRandomWord, playGame ) 
import Type ( GameState(GameState) ) 
import History
    ( logGameResult,
      processHistory,
      formatRecord,
      validateDeletion,
      readHistory ) 
import Report
    ( calculateTotalGames,
      calculateWins,
      calculateLosses,
      scoreValue,
      calculateBestScore,
      calculateTotalScore,
      calculateAverageScore,
      generateInsights )


-- Function to prompt for user input at the start of the game
getUserChoice :: IO Int
getUserChoice =
    putStrLn "\n--- LETTERS GAME ---" >>
    putStrLn "[1] Start Game" >>
    putStrLn "[2] View History" >>
    putStrLn "[3] View Instructions" >>
    putStrLn "[4] Generate Report" >>
    putStrLn "[5] Exit" >>
    putStrLn "Enter Your Choice: " >>
    getValidInput (\n -> n >= 1 && n <= 5) -- get and check user input validity 

-- Process user input 
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

startGame :: IO ()
startGame = do
    target <- selectRandomWord
    let initialState = GameState 6 target ()
    result <- playGame initialState
    logGameResult "game_records.txt" target result
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "Do you want to play again? \n[1] Yes | [2] Exit to Menu"
    choice <- getValidInput (\n -> n >= 1 && n <= 2)
    case choice of 
        1 -> startGame 
        2 -> main 


-- Function to view history
viewHistory :: IO ()
viewHistory = 
    processHistory "game_records.txt" main >>= \records ->
        unless (null records) $
            setSGR [SetConsoleIntensity BoldIntensity] >>
            putStrLn "\nYOUR GAME HISTORY:" >>
            setSGR [Reset] >>
            mapM_ (putStrLn . formatRecord) (zip [1 ..] records) >>
            putStrLn "\nWhich operation would you like to perform\n[1] Delete Record [2] Exit to Menu\nEnter your choice: " >>
            getValidInput (\n -> n >= 1 && n <= 2) >>= \input ->
                putStrLn "-----------------------------------------------------------------------\n" >>
                case input of
                    1 -> deleteRecords
                    2 -> main

-- Function to delete records
deleteRecords :: IO ()
deleteRecords =
    processHistory "game_records.txt" main >>= \records -> -- check if the file is empty
        unless (null records) ( -- if the file is not empty then perform the following
            setSGR [SetConsoleIntensity BoldIntensity] >> 
            putStrLn "DELETING YOUR GAME HISTORY\n" >>
            setSGR [Reset] >>
            traverse_ (putStrLn . formatRecord) (zip [1 ..] records) >> -- format and display the records details
            putStrLn "Enter the record number to delete or 0 to cancel:" >> -- get the index number user wants to delete
            getLine >>= \input ->
            case validateDeletion input records of -- check user input index exist in the file
                Left errMsg -> putStrLn errMsg >> viewHistory -- if does not exist, display error message and prompt for [1] Delete Records [2] Exit to menu
                Right updatedRecords -> -- if exist, delete the line and display the updated record
                    writeFile "game_records.txt" (unlines updatedRecords) >>
                    putStrLn "Record deleted successfully.\n-----------------------------------------------------------------------\nYour record has been UPDATED" >>
                    viewHistory
        )

-- Function to view instructions 
viewInstructions :: IO ()
viewInstructions =
    setSGR [SetConsoleIntensity BoldIntensity] >> -- set to bold text 
    putStrLn "\nHOW TO PLAY THE LETTERS GAME" >>
    setSGR [Reset] >>
    
    putStrLn "1. You will be given a random 5-letter word to guess." >>
    putStrLn "2. You have 6 attempts to guess the word correctly." >>
    putStrLn "3. After each guess, you'll receive feedback:" >>
    
    setSGR [SetColor Foreground Vivid Green] >> -- set to green colour
    putStrLn "  - Correct: The letter is in the correct position." >>
    
    setSGR [SetColor Foreground Vivid Yellow] >> -- set to yellow colour
    putStrLn "  - Misplaced: The letter is in the word but in the wrong position." >>
    
    setSGR [SetColor Foreground Vivid Red] >> -- set to red colour
    putStrLn "  - Incorrect: The letter does not exist in the word." >>
    setSGR [Reset] >>
    
    putStrLn "4. Your score is based on how quickly you guess the word:" >>
    putStrLn "  - 6 points for guessing correctly on the first attempt." >>
    putStrLn "  - 5 points for the second attempt" >>
    putStrLn "  - 4 points for the third, and so on." >>
    putStrLn "  - If you guess the word on the 6th attempt, you score 1 point." >>
    putStrLn "  - If you fail to guess the word within 6 attempts, you will earn 0 points." >>
    
    putStrLn "\nReady to Play?" >>
    putStrLn "Choose the number of the option you want from the menu to begin playing." >>
    putStrLn "[1] Start Game [2] Exit to Menu" >>
    putStrLn "Enter your choice: " >>
    
    getValidInput (\n -> n >= 1 && n <= 2) >>= \choice -> -- check for user's input validity 
    case choice of
        1 -> startGame
        2 -> main

-- Function to generate report
generateReport :: IO ()
generateReport = do
    records <- readHistory "game_records.txt"

    -- Calculate stats
    let totalGames = calculateTotalGames records
        wins = calculateWins records
        losses = calculateLosses totalGames wins
        bestScore = (scoreValue <$> calculateBestScore records)
        totalScore = calculateTotalScore records
        averageScore = calculateAverageScore (getSum totalScore) totalGames
        insights = generateInsights totalGames averageScore

    -- Define styled sections
    let sectionLine = "-----------------------------------------------------------------------"
        headerStyle = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
        subHeaderStyle = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
        resetStyle = setSGR [Reset]

    -- Display the formatted report
    headerStyle
    putStrLn "\nYOUR GAME REPORT: "
    resetStyle
    putStrLn sectionLine
    subHeaderStyle
    putStrLn $ "Total Games:    " <> show totalGames
    putStrLn $ "Wins:           " <> show wins
    putStrLn $ "Losses:         " <> show losses
    putStrLn $ "Best Score:     " <> maybe "No scores yet." show bestScore
    printf "Average Score:  %.2f\n" averageScore
    resetStyle
    putStrLn sectionLine
    putStrLn "\nInsights:"
    headerStyle
    putStrLn insights
    resetStyle
    putStrLn sectionLine

    -- Menu option
    putStrLn "\n[1] Exit to Menu"
    getValidInput (\n -> n == 1) >>= \choice -> 
        case choice of
            1 -> main

-- main function to run
main :: IO ()
main = getUserChoice >>= processUserInput -- prompt for user input and process it