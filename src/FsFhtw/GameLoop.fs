module GameLoop

open DomainSerializer
open Domain 
open Game

let printCards (prefix : string) (cards : Card list) =
    cards
    |> List.map (serializeCard) //TODO Serialize Card does not seem to produce expected output in the terminal: CardSuit is not rendered
    |> List.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun cardString -> printfn "%s%s" prefix cardString)

let generatePlayer(deck : ShuffledDeck) (communityCards : CommunityCards) (playerName : string) (position : int) =
    let playerCards, deck = deck |> dealHoleCards
    ({
        name = playerName; 
        position = position; 
        hand = createHand communityCards playerCards; 
        holeCards = playerCards
    }
    , deck)

let generateAiPlayers (deck : ShuffledDeck) (communityCards : CommunityCards) (numberOfPlayers : int) =
    let rec generatePlayerList(deck : ShuffledDeck) (communityCards : CommunityCards) (position : int) (players : Player list) =
        if position > 0 then
            let newPlayer, modifiedDeck = generatePlayer deck communityCards ("Player " + position.ToString()) position
            generatePlayerList modifiedDeck communityCards (position - 1) (newPlayer :: players)
        else
            (players, deck)
    generatePlayerList deck communityCards numberOfPlayers []

let playRound (numberOfPlayers : int) (model : State) : State = 

    let calculateGainOrLoss (players : Player list) (winner : Player list) (currentPlayer : Player) : int =
        if winner |> List.exists(fun(player) -> player.name = model.name) then
            numberOfPlayers * 100 / winner.Length
        else
            -1 * 100

    printfn "Shuffling Deck..."
    let deck = createDeck () |> shuffleDeck
    
    printfn "Dealing Community Cards..."
    let communityCards, deck = deck |> dealCommunityCards

    printfn "Dealing Player Cards..."
    let aiPlayers, deck = generateAiPlayers deck communityCards numberOfPlayers
    let player, _ = generatePlayer deck communityCards model.name (numberOfPlayers + 1)
    let players = player :: aiPlayers

    printfn "Results:"
    printCards "Community Cards: " communityCards
    players |> List.iter(fun(player) -> printCards (sprintf "%s yielded a %O with" player.name player.hand.rank) player.holeCards)
    
    let winner = evaluateWinner players
    printfn "%d winner(s): " winner.Length
    winner |> List.iter(fun(player) -> (printfn "%s won the game with %A" player.name player.hand.rank))
    {name = model.name; coins = model.coins + calculateGainOrLoss players winner player}

let setName(name : string) (model : State) : State =
    {name = name; coins = model.coins}

let resetCoins(coins : int) (model : State) : State =
    {name = model.name; coins = coins}

let update (msg : Message) (model : State) : State =
    match msg with
    | SetName name -> setName name model
    | StartGame numberOfPlayers-> playRound numberOfPlayers model
    | ResetCoins coins -> resetCoins coins model
    | _ -> model
