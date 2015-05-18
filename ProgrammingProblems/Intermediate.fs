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
type Number = { value: string; plusEdge: Number option; minusEdge: Number option; blankEdge: Number option }
let nine = { value = "9"; plusEdge = None; minusEdge = None; blankEdge = None }
let eight = { value = "8"; plusEdge = Some(nine); minusEdge = Some(nine); blankEdge = Some(nine) }
let seven = { value = "7"; plusEdge = Some(eight); minusEdge = Some(eight); blankEdge = Some(eight) }
let six = { value = "6"; plusEdge = Some(seven); minusEdge = Some(seven); blankEdge = Some(seven) }
let five = { value = "5"; plusEdge = Some(six); minusEdge = Some(six); blankEdge = Some(six) }
let four = { value = "4"; plusEdge = Some(five); minusEdge = Some(five); blankEdge = Some(five) }
let three = { value = "3"; plusEdge = Some(four); minusEdge = Some(four); blankEdge = Some(four) }
let two = { value = "2"; plusEdge = Some(three); minusEdge = Some(three); blankEdge = Some(three) }
let one = { value = "1"; plusEdge = Some(two); minusEdge = Some(two); blankEdge = Some(two) }

let prob1_allEqualing100 () =
    let thoseEqualing100 = new List<String>()

    let calculateTotal (equation: string) = 
        let equationList = equation.Split(' ') |> Array.toList
        let headAsInt list = Int32.Parse(List.head list)
        let rec calculateTotal runningTotal equationList =
            match equationList with
            | [] -> runningTotal
            | "+" :: rest -> calculateTotal 
                                (runningTotal + headAsInt rest)
                                (List.tail rest)
            | "-" :: rest -> calculateTotal 
                                (runningTotal - headAsInt rest) 
                                (List.tail rest)
            | n :: rest -> calculateTotal (Int32.Parse(n)) rest

        calculateTotal 0 equationList

    let rec depthFirstTraverse stringRepresentation tree =
        match tree.plusEdge with
        | Some(plusTree) -> depthFirstTraverse 
                                (stringRepresentation + " + " + plusTree.value)
                                plusTree
        | None -> ()

        match tree.minusEdge with
        | Some(minusTree) -> depthFirstTraverse 
                                (stringRepresentation + " - " + minusTree.value)
                                minusTree
        | None -> ()

        match tree.blankEdge with
        | Some(blankTree) -> depthFirstTraverse 
                                (stringRepresentation + blankTree.value)
                                blankTree
        | None -> ()

        if tree.plusEdge = None && (calculateTotal stringRepresentation) = 100 then 
            thoseEqualing100.Add(stringRepresentation)
        ()

    depthFirstTraverse "1" one

    thoseEqualing100.ToArray()
