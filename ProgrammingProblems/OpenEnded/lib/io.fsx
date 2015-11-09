module IO

#load "types.fsx"
#load "data.fsx"
open Types

open System
open System.IO

let readWordsFile () =
  File.ReadAllLines(Data.wordFilePath)

let rec getGuess userGameData =
  let line = Console.ReadLine()
  let guess = if line <> String.Empty
              then line.[0].ToString()
              else String.Empty

  if userGameData.guessedLetters.Contains(guess)
  then getGuess userGameData
  else guess

let clearScreen () =
  Console.Clear()

let printLost secretWord =
  printf "You Lost. You're a loser. I'm glad I'm not you.\n"
  printf "The word was: %s\n\n" secretWord

let printWon () =
  printf "You Won!. You should buy yourself a cookie.\n\n"

let printWordSoFar game =
  let wordSoFar = game.secretWord
                  |> String.collect (
                    fun swLetter ->
                      if game.guessedLetters |> String.exists ((=) swLetter)
                      then swLetter.ToString()
                      else Data.letterMask)
  printf "Secret Word: %s\n" wordSoFar

let printGuessed guessedLetters =
  printf "Guessed:     %s\n" guessedLetters

let printHangman (index) =
  printf "%s" Data.hangman.[index]

let printScreen game =
  printHangman game.incorrectCount
  printGuessed game.guessedLetters
  printWordSoFar game


