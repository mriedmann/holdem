module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|SetName|StartGame|ResetCoins|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; arg ] when safeEquals verb (nameof Domain.SetName) -> SetName arg
    | [ verb; arg ] when safeEquals verb (nameof Domain.StartGame) -> tryParseInt arg (fun value -> StartGame value)
    | [ verb; arg ] when safeEquals verb (nameof Domain.ResetCoins) -> tryParseInt arg (fun value -> ResetCoins value)
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed
