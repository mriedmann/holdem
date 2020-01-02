module Helpers

open System

let mutable rnd = Random()

let SeedRandom (seed:int) =
    rnd <- Random(seed)

let KnuthShuffle (lst : array<'a>) =                   // '
    let Swap i j =                                                  // Standard swap
        let item = lst.[i]
        lst.[i] <- lst.[j]
        lst.[j] <- item
    let ln = lst.Length
    [0..(ln - 2)]                                                   // For all indices except the last
    |> Seq.iter (fun i -> Swap i (rnd.Next(i, ln)))                 // swap th item at the index with a random one following it (or itself)
    lst   

let notImplemented () =
    raise (NotImplementedException())

type EvaluationResult<'TContinue,'TFinished> = 
    | Continue of 'TContinue
    | Finished of 'TFinished

let bind f =
    function
    | Continue s -> f s
    | Finished f -> Finished f

let (>>=) x f = 
    bind f x 

let hasFinished (result) =
    match result with
    | Finished _ -> true
    | _ -> false

let isNotEmpty (x : 'a list) =
    x |> (List.isEmpty >> not)

let compare (val1 : 'a) (val2 : 'a) : int =
    if val1 > val2 then 1
    else if val1 < val2 then -1
    else 0
