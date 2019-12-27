module Domain

type CardValue =
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
    CardValue * CardSuit

type Deck = 
    Card list

type ShuffledDeck = 
    Card list

type BoardCards =
    Card list

type Hand = 
    Card list

type ScoredCards = {
    board: BoardCards;
    hand: Hand
}

let dealCards(deck : ShuffledDeck) (number : int) =
    (deck.[0..number], deck.[number+1..deck.Length])

let dealHand(deck : ShuffledDeck) =
    let hand = dealCards deck, 2
    hand

let addCardsToBoard(board : BoardCards), (cards : Card list) =
    let additionalBoardCards = new BoardCards(cards)
    board :: additionalBoardCards

let dealFlop(deck : ShuffledDeck) =
    dealCards deck, 3

let dealTurn(deck : ShuffledDeck) (board : BoardCards) =
    let(additionalBoardCards, newDeck) = dealCards deck, 1
    (addCardsToBoard(board, additionalBoardCards), newDeck)

let dealRiver(deck : ShuffledDeck, board : BoardCards) =
    let(additionalBoardCards, newDeck) = dealCards deck, 1
    (addCardsToBoard(board, additionalBoardCards), newDeck)