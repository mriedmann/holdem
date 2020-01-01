module DomainSerializer

open Domain

let serializeCardRank (cardRank:CardRank) =
    match cardRank with
    | CardRank.Ace   -> 'A'
    | CardRank.King  -> 'K'
    | CardRank.Queen -> 'Q'
    | CardRank.Jack  -> 'J'
    | CardRank.Ten   -> 'T'
    | CardRank.Nine  -> '9'
    | CardRank.Eight -> '8'
    | CardRank.Seven -> '7'
    | CardRank.Six   -> '6'
    | CardRank.Five  -> '5'
    | CardRank.Four  -> '4'
    | CardRank.Three -> '3'
    | CardRank.Two   -> '2'
    | _ -> failwith "Unknown CardRank provided"

let deserializeCardRank (cardRankChar:char) =
    match cardRankChar with
    | 'A' -> CardRank.Ace  
    | 'K' -> CardRank.King 
    | 'Q' -> CardRank.Queen
    | 'J' -> CardRank.Jack 
    | 'T' -> CardRank.Ten  
    | '9' -> CardRank.Nine 
    | '8' -> CardRank.Eight
    | '7' -> CardRank.Seven
    | '6' -> CardRank.Six  
    | '5' -> CardRank.Five 
    | '4' -> CardRank.Four 
    | '3' -> CardRank.Three
    | '2' -> CardRank.Two
    | _ -> failwith "invalid CardRankChar provided"

let serializeCardSuit (cardSuit:CardSuit) =
    LanguagePrimitives.EnumToValue cardSuit

let serializeCard (card : Card) =
    sprintf "%c%c" (card |> (cardRank >> serializeCardRank)) (card |> (cardSuit >> serializeCardSuit))

let deserializeCard (cardString : char array) =
    let cardS:CardSuit = LanguagePrimitives.EnumOfValue cardString.[1]
    let cardR:CardRank = deserializeCardRank cardString.[0]
    Card (cardR, cardS)

let serializeDeck (deck : Deck) =
    deck |> List.map serializeCard |> List.reduce (+)

let deserializeDeck (deckString: string) =
    deckString
        |> Seq.windowed 2
        |> Seq.mapi (fun i j -> (i,j))
        |> Seq.filter (fun (i,j) -> i % 2=0)
        |> Seq.map (fun (_,j) -> deserializeCard j)
        |> List.ofSeq