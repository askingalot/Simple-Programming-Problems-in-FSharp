open Intermediate

[<EntryPoint>]
let main argv = 
    Intermediate.prob1_allEqualing100 ()
    |> Seq.iter (fun s -> printf "%s\n" s)

    0
