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

let compare (val1 : 'a) (val2 : 'a) : int =
    if val1 > val2 then 1
    else if val1 < val2 then -1
    else 0