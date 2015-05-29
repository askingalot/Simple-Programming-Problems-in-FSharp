module AdvancedTests

open FsUnit
open NUnit.Framework
open NUnitRunner

[<Test>]
let ``Prob 1 - single letter``() =
    Advanced.prob1_longestSubstring "a" "ab"
    |> should equal "a"

[<Test>]
let ``Prob 1 - double letter``() =
    Advanced.prob1_longestSubstring "ab" "aba"
    |> should equal "ab"

[<Test>]
let ``Prob 1 - multiple substrings``() =
    let result = Advanced.prob1_longestSubstring "abcabbaabb" "abcbbb"
    result 
    |> should equal "abc"

[<Test>]
let ``Prob 1 - deep substring``() =
    let result = Advanced.prob1_longestSubstring "xxxabca bbbb aabbbbxxx" "abcbbbb"
    result 
    |> should equal "bbbb"