#load "lib/data.fsx"
#load "lib/types.fsx"
#load "lib/io.fsx"

open System
open System.Threading

let secretWord words =
  let isInitCap (word: string) =
    let upperWord = word.ToUpper()
    word.[0] = upperWord.[0]

  words
  |> Seq.skip (Random().Next(Array.length words))
  |> Seq.find (fun w -> (String.length w) >= Data.minWordLength &&
                        (String.length w) <= Data.maxWordLength &&
                        not (isInitCap w))


let allGuessed guessedLetters word =
  word |> String.forall (fun w -> guessedLetters
                                  |> String.exists ((=) w))

let isWon (game: Types.GameState) =
  game.secretWord |> allGuessed game.guessedLetters

let containsGuess letter (word: string) =
  word.Contains(letter)

let isLost (game: Types.GameState) =
  game.incorrectCount = game.maxIncorrectCount



let rec gameLoop (game: Types.GameState) =
  IO.clearScreen()
  IO.printScreen game

  if game |> isLost then
    IO.printLost game.secretWord
    ()
  else if game |> isWon then
    IO.printWon ()
    ()
  else
    let guess =
      IO.getGuess { guessedLetters = game.guessedLetters
                  ; wordSoFar      = game.wordSoFar }

    if game.secretWord |> containsGuess guess then
      gameLoop { game with guessedLetters = game.guessedLetters + guess }
    else
      gameLoop { game with incorrectCount = game.incorrectCount + 1
                         ; guessedLetters = game.guessedLetters + guess }



gameLoop { secretWord        = secretWord (IO.readWordsFile ())
         ; guessedLetters    = ""
         ; wordSoFar         = ""
         ; incorrectCount    = 0
         ; maxIncorrectCount = (Array.length Data.hangman) - 1 }


