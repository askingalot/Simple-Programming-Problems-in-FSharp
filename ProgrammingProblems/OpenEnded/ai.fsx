module AI

#load "lib/types.fsx"
#load "lib/io.fsx"

open System
open System.Threading

type Player(allWords: string[]) =
  let letterChoices = Array.ofSeq "esrtalinybcdfghjkmopquvwxz"
  let mutable candidateWords = allWords

  member this.GetGuess (userGameData: Types.UserGameData) =
    let rec getGuess (letterChoices: char[]) =
      let randomIndex = Random().Next(letterChoices.Length)
      let guess = letterChoices.[randomIndex].ToString()

      if userGameData.guessedLetters.Contains(guess)
      then getGuess letterChoices
      else guess

(*
    printf "%A\n" userGameData
    printf "%d\n" candidateWords.Length
    printf "%A\n" candidateWords
*)

    candidateWords <-
      candidateWords
      |> Array.filter (
          fun candidate ->
            candidate.Length = userGameData.wordSoFar.Length )

    let letterChoices = Array.ofSeq (String.concat "" candidateWords)
    let guess = getGuess letterChoices
    Console.WriteLine(guess)
    Thread.Sleep(1000)
    //Console.ReadKey () |> ignore
    guess


let player = Player(IO.readWordsFile ())
let getGuess (userGameData: Types.UserGameData) =
  player.GetGuess(userGameData)

(*
let rec getGuess (userGameData: Types.UserGameData) =
  let line = Console.ReadLine()
  let guess = if line <> String.Empty
              then line.[0].ToString()
              else String.Empty

  if userGameData.guessedLetters.Contains(guess)
  then getGuess userGameData
  else guess
*)
