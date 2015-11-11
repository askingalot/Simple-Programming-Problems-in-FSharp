module AI

#load "lib/data.fsx"
#load "lib/types.fsx"
#load "lib/io.fsx"

open System
open System.Threading

type Player(allWords: string[]) =
  let lettersOrderedByFrequency letters =
    letters
    |> Seq.groupBy (fun l -> l)
    |> Seq.sortBy (fun (l, ls) -> (Seq.length ls) * -1)
    |> Seq.map (fun (l, ls) -> l)
    |> Array.ofSeq

  let filterCandidates (wordSoFar: string) (candidateWords: string[]) =
    candidateWords
    |> Array.filter (
        fun candidate ->
          candidate.Length = wordSoFar.Length
          &&
          (Seq.zip candidate wordSoFar)
           |> Seq.forall (fun (c, w) -> c = w || w = Data.letterMask))

  let rec getUnusedLetter (letterChoices: char[]) (guessedLetters: string) index =
    let guess = (letterChoices.[index]).ToString()

    if guessedLetters.Contains(guess)
    then getUnusedLetter letterChoices guessedLetters (index + 1)
    else guess

  member this.GetGuess (userGameData: Types.UserGameData) =

    let letterChoices =
      allWords
      |> filterCandidates userGameData.wordSoFar
      |> String.concat ""
      |> lettersOrderedByFrequency

    let guess = getUnusedLetter letterChoices userGameData.guessedLetters 0

    printf "%s" guess
    Thread.Sleep(1000)
    guess


