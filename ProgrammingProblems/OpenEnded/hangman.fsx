#load "lib/data.fsx"
#load "lib/types.fsx"
#load "lib/io.fsx"
#load "ai.fsx"

open System
open System.Threading

let getSecretWord words =
  let isInitCap (word: string) =
    let upperWord = word.ToUpper()
    word.[0] = upperWord.[0]

  words
  |> Seq.skip (Random().Next(Array.length words))
  |> Seq.find (fun w -> (String.length w) >= Data.minWordLength &&
                        (String.length w) <= Data.maxWordLength &&
                        not (isInitCap w))


let allGuessed guessedLetters word =
  word |> String.forall (fun w -> String.exists ((=) w) guessedLetters)

let isWon (game: Types.GameState) =
  game.secretWord |> allGuessed game.guessedLetters

let containsGuess letter (word: string) =
  word.Contains(letter)

let isLost (game: Types.GameState) =
  game.incorrectCount = game.maxIncorrectCount

let getWordSoFar guessedLetters word =
  word |> String.collect (fun w ->
                            if String.exists ((=) w) guessedLetters
                            then w.ToString()
                            else Data.letterMask )


let rec gameLoop (game: Types.GameState) =
  IO.clearScreen()
  IO.printScreen game

  if game |> isLost then
    IO.printLost game.secretWord
  else if game |> isWon then
    IO.printWon ()
  else
    let guess = AI.getGuess { guessedLetters = game.guessedLetters
                            ; wordSoFar      = game.wordSoFar }

    let guessedLetters = game.guessedLetters + guess
    let wordSoFar = getWordSoFar guessedLetters game.secretWord

    if game.secretWord |> containsGuess guess then
      gameLoop { game with guessedLetters = guessedLetters
                         ; wordSoFar      = wordSoFar }
    else
      gameLoop { game with incorrectCount = game.incorrectCount + 1
                         ; guessedLetters = guessedLetters
                         ; wordSoFar      = wordSoFar }


let secretWord = getSecretWord (IO.readWordsFile ())
gameLoop { secretWord        = secretWord
         ; guessedLetters    = ""
         ; wordSoFar         = getWordSoFar "" secretWord
         ; incorrectCount    = 0
         ; maxIncorrectCount = (Array.length Data.hangman) - 1 }


