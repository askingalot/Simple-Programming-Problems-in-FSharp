module Advanced

(*
Given two strings, write a program that efficiently 
finds the longest common subsequence.
*)
let prob1_longestSubstring (string1: char seq) (string2: char seq) =
    let chars1 = string1 |> List.ofSeq 
    let chars2 = string2 |> List.ofSeq

    let rec findMachingChars chars1 chars2 index lastMatchingIndex result =
        match chars1, chars2 with
        | [], _ | _, [] -> result
        | h1 :: t1, h2 :: t2 when h1 = h2 -> 
            if index-1 = lastMatchingIndex then
                let lastMatchingResult = List.head result
                let otherMatches = List.tail result
                findMachingChars t1 t2 (index+1) index 
                    ((h1 :: lastMatchingResult) :: otherMatches)
            else 
                findMachingChars t1 t2 (index+1) index ([h1] :: result)
        | h1 :: t1, h2 :: t2 ->
            findMachingChars t1 t2 (index+1) lastMatchingIndex result

    let rec moveAccrossStrings chars1 chars2 result =
        match chars1 with 
        | [] -> result
        | _ -> 
            findMachingChars chars1 chars2 0 -99 result 
            |> moveAccrossStrings (List.tail chars1) chars2

    moveAccrossStrings chars1 chars2 []
    |> List.maxBy List.length
    |> List.rev



(*
Write a program that automatically converts English text to Morse code and vice versa.
*)
(*
I'm going with international Morse code as described by wikipeida:
https://en.wikipedia.org/wiki/Morse_code
https://en.wikipedia.org/wiki/Morse_code#/media/File:International_Morse_Code.svg
*)
type MorseCode = 
    | Dot 
    | Dash
    | Space


let prob6_englishToMorseCode (englishText : string) =
    let words = englishText.Trim().ToUpper().Split(' ') |> List.ofArray

    let charToMorse = function 
        | 'A' -> [Dot; Dash]
        | 'B' -> [Dash; Dot; Dot; Dot]
        | 'C' -> [Dash; Dot; Dash; Dot]
        | 'D' -> [Dash; Dot; Dot]
        | 'E' -> [Dot]
        | 'F' -> [Dot; Dot; Dash; Dot]
        | 'G' -> [Dash; Dash; Dot]
        | 'H' -> [Dot; Dot; Dot; Dot]
        | 'I' -> [Dot; Dot]
        | _ -> failwith "I didn't feel like doing any more"
        
    let rec intersperse sep = function
        | [] -> []
        | last :: [] -> [last]
        | h :: t -> h :: sep :: intersperse sep t

    words 
    |> List.map (
        fun word ->
            word.ToCharArray()
            |> List.ofArray
            |> List.map charToMorse
            |> intersperse [Space; Space; Space]
            |> List.concat)
    |> intersperse [Space; Space; Space; Space; Space; Space; Space]
    |> List.concat



