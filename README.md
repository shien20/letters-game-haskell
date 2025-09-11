# 🔡 Letters Game (Haskell)

A word-based puzzle game inspired by **Wordle**, built entirely in **Haskell** to demonstrate functional programming concepts.  
The player must guess a hidden word within a limited number of attempts, with feedback provided after each guess.  

---

## 🎮 Gameplay
- The game randomly selects a hidden word.  
- The player has a limited number of guesses to find the correct word.  
- After each guess:
  - ✅ Correct letters in the right position are highlighted.  
  - 🔄 Correct letters in the wrong position are indicated.  
  - ❌ Incorrect letters are marked as invalid.  
- The game ends when:
  - The player guesses the word correctly 🎉  
  - OR the guess limit is reached ❌  

---

## ✨ Features
- Functional programming principles applied (recursion, pattern matching, pure functions).  
- Immutable state handling of game progress.  
- Randomized word selection for replayability.  
- Simple terminal/console-based UI.  

---

## 🛠️ Tech Stack
- **Language**: Haskell  
- **Concepts**: Functional Programming, Recursion, Pattern Matching  
- **Build Tool**: Cabal  

---

## ⚙️ Installation & Running
1. Clone the repository:
   ```bash
   git clone https://github.com/shien20/letters-game-haskell.git
   cd letters-game
