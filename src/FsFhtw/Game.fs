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
    (deck.[0..number], deck.[number+1..deck.Length])

let dealHoleCards : DealHoleCards = fun deck player ->
    (deck, {cards = List.Empty; player = player})
    (* 
    let remainingDeck, cards = dealCards deck 2
    (remainingDeck, {cards = cards; player = player})
    *)

let dealCommunityCards : DealCommunityCards = fun deck ->
    (deck, List.Empty)
    (* dealCards deck 5 *)

let evaluateWinner : EvaluateWinner = fun players ->
    players |> List.head 

let createHand : CreateHand = fun comCards holeCards ->
    {
        rank = HandRank.HighCard
        rankValue = CardRank.Two
        kicker = None
        player = holeCards.player
    }

let compareHands : CompareHands = fun hand1 hand2 ->
    None