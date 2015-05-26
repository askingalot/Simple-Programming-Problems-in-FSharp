module Advanced

(*
Given two strings, write a program that efficiently 
finds the longest common subsequence.
*)
let prob1_longestSubstring (string1: char seq) (string2: char seq) =
    let chars1 = string1 |> List.ofSeq 
    let chars2 = string2 |> List.ofSeq

    let rec findMachingChars chars1 chars2 index (lastMatchingIndex, result) =
        match chars1, chars2 with
        | [], _ | _, [] -> result
        | h1 :: t1, h2 :: t2 when h1 = h2 -> 
            if index-1 = lastMatchingIndex then
                let lastMatchingResult = List.head result
                let otherMatches = List.tail result
                findMachingChars t1 t2 (index+1) 
                    (index, (h1 :: lastMatchingResult) :: otherMatches)
            else 
                findMachingChars t1 t2 (index+1) 
                    (index, [h1] :: result)
        | h1 :: t1, h2 :: t2 ->
            findMachingChars t1 t2 (index+1) (lastMatchingIndex, result)

    let results = findMachingChars chars1 chars2 0 (-99, [])
    results 
    |> List.maxBy List.length
    |> List.rev