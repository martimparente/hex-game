# 🎮 Hex Game

A strategic board game implementation with AI players using Minimax and Alpha-Beta pruning algorithms using Prolog.

## 🎲 About the Game

Hex is a two-player abstract strategy board game in which players attempt to connect opposite sides of a rhombus-shaped board made of hexagonal cells. The game was invented by mathematician and poet Piet Hein in 1942 and later rediscovered and popularized by John Nash.


[Learn more about Hex on Wikipedia](https://en.wikipedia.org/wiki/Hex_(board_game))

## 📋 Features

### Version 1.0.0
- ✨ Two-player support for any board dimension
- 🔄 Refactored game loops with configurable first player
- 🧠 Minimax algorithm implementation
- 🧮 Alpha-Beta pruning optimization
- 🤖 CPU vs CPU gameplay option
- ✅ Tested configurations:
  - 2x2 board
  - 3x3 board
  - 4x4 board (with Alpha-Beta)
  - After 5x5 board, AI takes too long to make a move


## 📋 Requirements

- [SWI-Prolog](https://www.swi-prolog.org/download/stable) (version 9.0.0 or higher recommended)

## 🚀 Getting Started


### Run the Game
```bash
swipl .\hex.pl
```

### Tests

#### DFS Test
```bash
swipl .\tests\test_dfs.pl
```

#### MiniMax Test
```bash
swipl .\tests\test_minimax.pl
```

#### AlphaBeta Test
```bash
swipl .\tests\test_alphabeta.pl
```

