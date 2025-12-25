# Karma Game

**Karma** is a Haskell-based card game simulation, offering both a basic and advanced set of rules, including special card effects and unique strategy implementations. The game allows for dynamic multiplayer experiences, including AI opponents with various strategic behaviors. Optional rule extensions like **Reverse 8**, **Three 3s**, and **Nine of Clubs** add extra layers of strategy, making it a highly customizable and engaging game.

## Features
- **Card Mechanics**: Core rules including legal plays, burns, and special effects.
- **Player Strategy**: AI players with basic and advanced strategies that adapt based on game state.
- **Extensions**: Optional rule extensions like Reverse 8 and Nine of Clubs that alter gameplay.
- **Game History**: Tracks detailed turn-by-turn history for debugging or strategy analysis.
- **Multiplayer**: Supports multiple human or AI players in a dynamic game loop.

## Requirements
- **Haskell (GHC)**
- **mtl package**

## Setup

1. Clone the repository:
    ```bash
    git clone https://github.com/yourusername/karma-game.git
    ```

2. Install Haskell (GHC).

3. The `mtl` package is included with GHC, but you can verify it's available.

## Run (Quick, Interactive)

Load the module in GHCi with the `mtl` package:

```powershell
ghci -package mtl Karma.hs
```

At the `ghci` prompt you can call any of the following top-level IO actions:

```haskell
-- Start a single interactive game
playOneGame

-- Run one game and return detailed history
runOneGameWithHistory

-- Alternate single-game entrypoint (step 4 rules / variant)
playOneGameStep4

-- Run a tournament between AI players
playTournament
```

**Notes:**
- Type the command name and press Enter at the `ghci` prompt, e.g. `> playOneGame`
- Use `:reload` if you edit `Karma.hs` while GHCi is open
- Use `:quit` to exit GHCi

## Run (Compile to Executable)

Compile an executable with the `mtl` package:

```powershell
ghc --make Karma.hs -package mtl -o Karma.exe
.\Karma.exe
```

## Game Flow

The game proceeds with players taking turns, where each player attempts to play cards based on the legal play rules or special card effects. The game loop continues until only one player remains.

- **Basic Strategy**: AI players make legal card plays based on their hand and face-up cards.
- **Advanced Strategy**: AI prioritizes completing four-of-a-kinds, burning large piles, or playing sets of cards.
- **Game History**: Detailed logs are kept for each turn, showing player actions and game effects.

## Customizing Extensions

You can modify the game's behavior by enabling or disabling optional **extensions**. Extensions can be specified when running the game and include:

- **Reverse 8**: Reverses the order of play when a card of rank 8 is played.
- **Three 3s**: Forces the next player to pick up the discard pile when three 3s are played.
- **Nine of Clubs**: Steals a card from the next player when the Nine of Clubs is played.
