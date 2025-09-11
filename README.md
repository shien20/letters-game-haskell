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

### Prerequisites
- Install [Haskell Platform](https://www.haskell.org/platform/) (this includes GHC and Cabal).  

---

### Steps
1. **Download or clone the repository**
   - Click the green **Code** button on GitHub → *Download ZIP*  
   - Or run:
     ```bash
     git clone https://github.com/shien20/letters-game-haskell.git
     ```

2. **Open the project folder**
   - Extract the ZIP (if downloaded).  
   - Open the folder named `letters-game-haskell`.  

3. **Open Command Prompt in the folder**
   - Click on the folder path bar at the top.  
   - Type `cmd` and press **Enter**.  
   - A Command Prompt window will open in this folder location.  

4. **Build the project**
   ```bash
   cabal build

5. **Run the game**
   ```bash
   cabal run

6. **Enjoy!**
   🎉 The game will start, and you can play directly in the Command Prompt window.
