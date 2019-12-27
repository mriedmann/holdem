module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Help|ParseFailed|) (input : string) =
    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed
