module ListsAndStringsTests

open FsUnit
open NUnit.Framework
open NUnitRunner

[<Test>]
let ``Prob 7 - for loop test``() =
    ListsAndStrings.prob7_forLoopSum [1; 1; 1] 
    |> should equal 3

[<Test>]
let ``Prob 7 - while loop test``() =
    ListsAndStrings.prob7_whileLoopSum [1; 1; 1] 
    |> should equal 3

[<Test>]
let ``Prob 7 - recursion test``() =
    ListsAndStrings.prob7_recursiveSum [1; 1; 1] 
    |> should equal 3


[<Test>]
let ``Prob 10 - myZip test``() =
    ListsAndStrings.prob10_myZip [1; 3; 5] [2; 4; 6]
    |> should equal [1; 2; 3; 4; 5; 6]

[<Test>]
let ``Prob 12 - 100 Fibonacci test``() =
    ListsAndStrings.prob12_firstHundredFibonacci () 
    |> Seq.take 10
    |> should equal [0; 1; 1; 2; 3; 5; 8; 13; 21; 34 ]


[<Test>]
let ``Prob 17 - Put words in a box``() =
    ListsAndStrings.prob17_textInABox ["Hello"; "World"; "in"; "a"; "frame"] 
    |> should equal "*********
* Hello *
* World *
* in    *
* a     *
* frame *
*********
"