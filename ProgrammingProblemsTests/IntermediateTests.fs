﻿module IntermediateTests

open FsUnit
open NUnit.Framework
open NUnitRunner


[<Test>]
let ``Prob 1 - All equations that equal 100``() =
    let expectedEquations = [|
        "1 + 2 + 3 - 4 + 5 + 6 + 78 + 9";
        "1 + 2 + 34 - 5 + 67 - 8 + 9";
        "1 + 23 - 4 + 5 + 6 + 78 - 9"
        "1 + 23 - 4 + 56 + 7 + 8 + 9";
        "12 + 3 + 4 + 5 - 6 - 7 + 89";
        "12 + 3 - 4 + 5 + 67 + 8 + 9";
        "12 - 3 - 4 + 5 - 6 + 7 + 89";
        "123 + 4 - 5 + 67 - 89";
        "123 + 45 - 67 + 8 - 9";
        "123 - 4 - 5 - 6 - 7 + 8 - 9";
        "123 - 45 - 67 + 89" |]

    Intermediate.prob1_allEqualing100 ()
    |> should equal expectedEquations
