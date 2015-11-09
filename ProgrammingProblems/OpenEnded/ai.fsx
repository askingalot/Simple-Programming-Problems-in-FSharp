module AI

#load "lib/types.fsx"
#load "lib/io.fsx"

open System

type Player(allWords: string[]) =
  member this.Words = allWords
  member this.GetGuess (userGameData: Types.UserGameData) =
    "a"

let rec getGuess (userGameData: Types.UserGameData) =
  let line = Console.ReadLine()
  let guess = if line <> String.Empty
              then line.[0].ToString()
              else String.Empty

  if userGameData.guessedLetters.Contains(guess)
  then getGuess userGameData
  else guess
