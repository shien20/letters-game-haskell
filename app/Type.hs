{-# LANGUAGE InstanceSigs #-}
module Type where 

-- custom data type to represent the state of the game.
data GameState a = GameState
    { remainingAttempts :: Int -- The number of remaining attempts for the player.
    , targetWord :: String -- The target word the player is trying to guess.
    , stateValue :: a -- holds additional state information
    } deriving Show

-- custom data type to represents the result of the game
-- A Win has a Score, while lose has nothing (0 point)
data GameResult = Lose | Win Score deriving (Show, Eq) 

-- represent a player's score in the game
newtype Score = Score Int deriving (Show, Eq, Ord)

-- custom data type to represent the correctness of the letter in the game
data Feedback = Correct | Misplaced | Incorrect deriving (Show, Eq)


-- Type class for operations that can be performed on the GameState.
-- contains functions to manipulate the game state
class GameStateOps s where
    decrementAttempts :: s -> s             -- Reduce the remaining attempts
    setTargetWord     :: String -> s -> s   -- Set the target word
    getRemainingAttempts :: s -> Int        -- Get the number of remaining attempts
    getTargetWord     :: s -> String        -- Get the target word


-- Implement GameState data type with GameStateOps class 
-- Enable us to manipulate GameState
instance GameStateOps (GameState a) where

    -- Decreases the remaining attempts by 1
    decrementAttempts :: GameState a -> GameState a
    decrementAttempts (GameState attempts target val) =
        GameState (attempts - 1) target val

    -- Sets the target word in the GameState.
    setTargetWord :: String -> GameState a -> GameState a
    setTargetWord word (GameState attempts _ val) =
        GameState attempts word val

    -- Retrieves the number of remaining attempts from the GameState.
    getRemainingAttempts :: GameState a -> Int
    getRemainingAttempts (GameState attempts _ _) = attempts

    -- Retrieves the target word from the GameState.
    getTargetWord :: GameState a -> String
    getTargetWord (GameState _ target _) = target


-- Implement Semigroup on Score
-- adds 2 scores together using <>
instance Semigroup Score where
    (<>) :: Score -> Score -> Score
    (Score a) <> (Score b) = Score (a + b)

-- Implement Monoid on Score
-- Define mempty and mappend to combine score
instance Monoid Score where
    mempty :: Score
    mempty = Score 0
    mappend :: Score -> Score -> Score
    mappend = (<>)


-- Implementing Functor, Applicative, Monad on GameState

-- mapping a function over the `stateValue` part of the GameState, leaving other fields unchanged.
instance Functor GameState where
    fmap :: (a -> b) -> GameState a -> GameState b
    fmap f (GameState attempts target val) = GameState attempts target (f val)

-- pure for creating a GameState with a default value
-- <*> for applying functions inside GameState.
instance Applicative GameState where
    pure :: a -> GameState a
    pure x = GameState 6 "" x
    (<*>) :: GameState (a -> b) -> GameState a -> GameState b
    (GameState _ _ f) <*> (GameState attempts target val) =
        GameState attempts target (f val)

-- to sequence actions that manipulate game state
instance Monad GameState where
    return :: a -> GameState a
    return = pure

    -- allows chaining operation to produce new GameState value
    (>>=) :: GameState a -> (a -> GameState b) -> GameState b
    (GameState attempts target val) >>= f =
        let (GameState newAttempts newTarget newVal) = f val
         in GameState (min attempts newAttempts) (if null target then newTarget else target) newVal
