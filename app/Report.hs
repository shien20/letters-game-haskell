module Report where

import Data.Semigroup (Sum(..))
import Text.Printf ( printf )
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)

import Type ( Score(..) )

-- calculate the total number of games user player
calculateTotalGames :: [String] -> Int
calculateTotalGames = length

-- calculate the total wins 
calculateWins :: [String] -> Int
calculateWins = length . filter ("Win" `isInfixOf`) -- only count the entries that contains "Win"

-- calculate total loses
calculateLosses :: Int -> Int -> Int
calculateLosses totalGames wins = totalGames - wins 

-- calculate total score 
calculateTotalScore :: [String] -> Sum Int
calculateTotalScore = foldMap (Sum . scoreValue) . mapMaybe parseScore -- extract the score and sum the scires

-- find the highest score
calculateBestScore :: [String] -> Maybe Score
calculateBestScore = foldr (maxScore . parseScore) Nothing 
  where
    -- compare two "Maybe Score" data type and return the higher one
    maxScore :: Maybe Score -> Maybe Score -> Maybe Score
    maxScore Nothing y = y
    maxScore x Nothing = x
    maxScore (Just a) (Just b) = Just (max a b)

-- calculate the average, where total score/ total games
calculateAverageScore :: Int -> Int -> Double
calculateAverageScore totalScore totalGames =
    if totalGames > 0
        then fromIntegral totalScore / fromIntegral totalGames
        else 0 -- return 0 if no games player

-- display all the calculations to user
generateInsights :: Int -> Double -> String
generateInsights totalGames averageScore =
    "You scored an average of " <> printf "%.2f" averageScore <>
    " points from a total of " <> show totalGames <> " games."


-- Extract the integer value from a `Score` type
scoreValue :: Score -> Int
scoreValue (Score s) = s

-- Extract the score if the String contains "Win"
parseScore :: String -> Maybe Score
parseScore record
    | "Win" `isInfixOf` record = 
        let scoreStr = last (words record) -- Extracts the last element which is score
         in Score <$> readMaybe scoreStr -- Converts the score to an integer if valid.
    | otherwise = Nothing

-- safely convert a String into a given type
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val -- Successful parsing with no leftover input.
    _           -> Nothing -- Parsing failed or leftover input exists.