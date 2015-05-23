module ListsAndStrings

open System
open System.Collections.Generic
open System.Text.RegularExpressions

// https://blog.svpino.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

(**
Write three functions that compute the sum of the numbers 
in a given list using a for-loop, a while-loop, and recursion.
*)
let prob7_forLoopSum numbers = 
    let mutable sum = 0
    for n in numbers do
        sum <- sum + n
    sum

let prob7_whileLoopSum numbers = 
    let arNumbers = Array.ofList numbers
    let mutable i = 0
    let mutable sum = 0
    while i < arNumbers.Length do
        sum <- sum + arNumbers.[i]
        i <- i + 1
    sum

let rec prob7_recursiveSum l =
    match l with 
    | [] -> 0
    | h :: t -> h + prob7_recursiveSum t


(**
Write a function that combines two lists by alternatingly 
taking elements. 
For example: given the two lists [a, b, c] and [1, 2, 3], 
the function should return [a, 1, b, 2, c, 3].
*)
let prob10_myZip alist blist =
    List.fold2 (
        fun result a b -> List.append result [a; b]
    ) [] alist blist


(*
Write a function that computes the list of the first 
100 Fibonacci numbers. By definition, the first two 
numbers in the Fibonacci sequence are 0 and 1, 
and each subsequent number is the sum of the previous two. 
As an example, here are the first 10 Fibonnaci numbers: 
0, 1, 1, 2, 3, 5, 8, 13, 21, and 34.
*)

let prob12_firstHundredFibonacci () =
    let rec fib n m = seq {
        yield n
        yield! (fib m (n + m))
    }
    fib 0L 1L |> Seq.take 100


(*
Write a function that takes a list of strings an prints 
them, one per line, in a rectangular frame. 
For example the list ["Hello", "World", "in", "a", "frame"] 
gets printed as:

*********
* Hello *
* World *
* in    *
* a     *
* frame *
*********
*)

let prob17_textInABox (words : string list) = 
    let longestWord = words |> List.maxBy (fun w -> w.Length)

    let boxWidth = longestWord.Length + 4
    let topOrdbottom = (String.replicate boxWidth "*") + "\r\n"

    let boxMiddle =
        words 
        |> List.fold (fun result word -> 
                        let paddedWord = word.PadRight(longestWord.Length)
                        result + (sprintf "* %s *\r\n" paddedWord)) ""

    String.Concat(topOrdbottom, boxMiddle, topOrdbottom)
