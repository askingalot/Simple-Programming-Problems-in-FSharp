module Types

type GameState = { secretWord: string
                 ; guessedLetters: string
                 ; wordSoFar: string
                 ; incorrectCount: int
                 ; maxIncorrectCount: int }

type UserGameData = { guessedLetters: string
                    ; wordSoFar: string }
