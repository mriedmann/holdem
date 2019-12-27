module Domain

type Player = {
    name : string;
    position : int 
}

type CardRank =
    | Ace = 14
    | King = 13
    | Queen = 12
    | Jack = 11
    | Ten = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2
    //TODO: Ace can also represent the value one for Straigths

type CardSuit =
    | Hearts = '\u2665'
    | Diamond = '\u2666'
    | Spade = '\u2660'
    | Club = '\u2663'

type Card = 
    CardRank * CardSuit

type Deck = 
    Card list

type ShuffledDeck = 
    Card list

type CommunityCards =
    Card list

type HoleCards = {
    cards : Card list;
    player : Player
}

type Hand = {
    board : CommunityCards;
    holeCard : HoleCards
}

let dealCards(deck : ShuffledDeck) (number : int) =
    (deck.[0..number], deck.[number+1..deck.Length])

let dealHoleCards(deck : ShuffledDeck) =
    dealCards deck 2

let dealCommunityCards(deck : ShuffledDeck) =
    dealCards deck 5

(*let dealFlop(deck : ShuffledDeck) =
    dealCards deck 3

let dealTurn(deck : ShuffledDeck) (board : CommunityCards) =
    let (additionalBoardCards, newDeck) = dealCards deck 1
    (addCardsToCommunityCards board additionalBoardCards, newDeck)

let dealRiver(deck : ShuffledDeck) (board : CommunityCards) =
    let(additionalBoardCards, newDeck) = dealCards deck 1
    (addCardsToCommunityCards board additionalBoardCards, newDeck) *)