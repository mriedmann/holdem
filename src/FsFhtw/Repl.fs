module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State

let read (input : string) =
    match input with
    | StartGame numberOfPlayers -> Domain.StartGame numberOfPlayers |> DomainMessage 
    | SetName name -> Domain.SetName name |> DomainMessage
    | ResetCoins amount -> Domain.ResetCoins amount |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update : Domain.Message -> State -> State) (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let message = sprintf "Ok, %A. Your current coin amount is %d. What now?" newState.name newState.coins
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : State) =
    Console.ReadLine()
    |> read
    |> evaluate GameLoop.update state
    |> print
    |> loop
