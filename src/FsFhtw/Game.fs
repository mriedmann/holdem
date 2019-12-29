module Game

open Domain 
open Helpers

let createDeck : CreateDeck = fun () ->
    let cardRanks = [ 
        CardRank.Two; CardRank.Three; CardRank.Four;
        CardRank.Five; CardRank.Six; CardRank.Seven;
        CardRank.Eight; CardRank.Nine; CardRank.Ten;
        CardRank.Jack; CardRank.Queen; CardRank.King;
        CardRank.Ace;
    ]
    [ for i in 0 .. 12 -> (cardRanks.[i], CardSuit.Club)] @
    [ for i in 0 .. 12 -> (cardRanks.[i], CardSuit.Diamond)] @
    [ for i in 0 .. 12 -> (cardRanks.[i], CardSuit.Hearts)] @
    [ for i in 0 .. 12 -> (cardRanks.[i], CardSuit.Spade)] 

let shuffleDeck : ShuffleDeck = 
    List.toArray >> KnuthShuffle >> Array.toList

let dealCards(deck : ShuffledDeck) (number : int) =
    deck |> List.splitAt (number)

let dealHoleCards : DealHoleCards = fun deck ->
    dealCards deck 2

let dealCommunityCards : DealCommunityCards = fun deck ->
    dealCards deck 5

let evaluateWinner : EvaluateWinner = fun players ->
    notImplemented ()

let createHand : CreateHand = fun comCards holeCards ->
    notImplemented ()

let compareHands : CompareHands = fun hand1 hand2 ->
    notImplemented ()