# sicp-leaderboard-bot-hs
Telegram bot that checks github accounts for existence of SICP exercises

## Stack
- Haskell (GHC 8.6.5) && cabal-install 2.4.1.0
- SQLite 3.29.0

Use [ghcup](https://github.com/haskell/ghcup/) to build

### How it works
Bot just counts a number of files with *.scm or *.rkt (Scheme or Racket)
extensions

### TODO
- Better exception handling
- More user-friendly interface
