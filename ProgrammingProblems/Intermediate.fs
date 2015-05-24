module Intermediate

open System
open System.Collections.Generic
open System.Text.RegularExpressions


(*
Write a program that outputs all possibilities to put + or - or nothing 
between the numbers 1, 2, ..., 9 (in this order) such that the 
result is always 100. 
For example: 1 + 2 + 34 – 5 + 67 – 8 + 9 = 100.
*)
type NumberNode = { value: string; next: NumberNode option }
let numberList = 
    [8..-1..1]
    |> List.fold (fun nextNode curNumber -> 
                    { value = (string curNumber); next = Some(nextNode) } )
                 { value = "9"; next = None }

let prob1_allEqualing100 () =
    let thoseEqualing100 = new List<String>()

    let calculateTotal (equation: string) = 
        let headAsInt list = Int32.Parse(List.head list)
        let rec calculateTotal runningTotal = function
            | [] -> runningTotal
            | "+" :: rest -> calculateTotal 
                                (runningTotal + headAsInt rest)
                                (List.tail rest)
            | "-" :: rest -> calculateTotal 
                                (runningTotal - headAsInt rest) 
                                (List.tail rest)
            | n :: rest -> calculateTotal (Int32.Parse(n)) rest

        calculateTotal 
            0 
            (equation.Split(' ') |> List.ofArray)

    let rec depthFirstTraverse stringRepresentation tree =
        match tree.next with
        | Some(next) -> 
            depthFirstTraverse (stringRepresentation + " + " + next.value) next
            depthFirstTraverse (stringRepresentation + " - " + next.value) next
            depthFirstTraverse (stringRepresentation + next.value) next
        | None when (calculateTotal stringRepresentation) = 100 ->
            thoseEqualing100.Add(stringRepresentation)
        | _ -> ()

    depthFirstTraverse "1" numberList

    thoseEqualing100.ToArray()
