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

let isNotEmpty (list : 'a list) : bool =
    match list with
        | [] -> false
        | _ -> true

type KindSize =
    | Pair = 2
    | Triple = 3
    | Quadruple = 4

type CardAppearance =
    | Once = 1
    | Twice = 2

let createHand : CreateHand = fun  (communityCards : CommunityCards) (holeCards : HoleCards) ->

    let cards : Card list = communityCards @ holeCards
    let orderedCards : Card list = cards |> List.sortByDescending(fun (cardRank, cardSuit) -> cardRank)

    let getCardsOfSuit (cards: Card list) (suit : CardSuit) : Card list =
        cards |> List.filter (fun (cardRank, cardSuit) -> cardSuit = suit)

    let determineFlushCards (cards : Card list) : Card list =
        let hearts = getCardsOfSuit cards CardSuit.Hearts
        let diamonds = getCardsOfSuit cards CardSuit.Diamond
        let spades = getCardsOfSuit cards CardSuit.Spade
        let clubs = getCardsOfSuit cards CardSuit.Club

        match cards with
            | c when hearts.Length >= 5 -> hearts
            | c when diamonds.Length >= 5 -> diamonds
            | c when spades.Length >= 5 -> spades
            | c when clubs.Length >= 5 -> clubs
            | _ -> List.Empty

    let isFlush (cards : Card list ) : bool =
        let flushCards = determineFlushCards cards
        match flushCards with
            | [] -> false
            | _ -> true

    let cardRankGroups : (CardRank * Card list) list =
        orderedCards 
            |> List.groupBy (fun (cardRank, cardSuit) -> cardRank) 
            
    let determineCardsOfAKind (kindSize : KindSize) (appearance : CardAppearance) : Card list =
        let filteredCardRankGroups =
            cardRankGroups
                |> List.filter (fun (cardRank, cardList) -> cardList.Length = int kindSize)
        if filteredCardRankGroups.Length = int appearance 
            then filteredCardRankGroups |> List.collect (fun (cardRank, cardList) -> cardList)
        else List.Empty

    let areCardsInStraight (cards : Card list) =
        let hasNextCardExpectedRank (currentCardRank : CardRank) (nextCardRank : CardRank) = 
            (int nextCardRank = int currentCardRank - 1)

        cards
            |> List.pairwise
            |> List.forall (fun ((currentCardRank, _), (nextCardRank, _)) -> 
                hasNextCardExpectedRank currentCardRank nextCardRank)
    
    let determineStraightCards (orderedCards : Card list) : Card list =
        let distinctOrderedCards = 
            orderedCards
            |> List.distinctBy(cardRank)
        
        if distinctOrderedCards.Length >= 5 then
            // TODO consider special case ACE-ONE here
            let bottomToTopOrderedCards = 
                distinctOrderedCards
                |> List.sortBy(cardRank)
                |> List.take 5
                |> List.sortDescending

            let topToBottomOrderedCards = 
                distinctOrderedCards
                |> List.take 5

            // for more than five cards, checks for a straight has to be done top down and vica versa
            if areCardsInStraight bottomToTopOrderedCards then bottomToTopOrderedCards else
            if areCardsInStraight topToBottomOrderedCards then topToBottomOrderedCards else
            List.Empty
        else
            List.Empty

    let isHighestStraight (cards : Card list) : bool =
        let headRank, _ = cards.Head
        let tailRank, _ = cards |> List.last
        headRank = CardRank.Ace && tailRank = CardRank.Ten

    let determineRankValue (cards : Card list) : CardRank =
        cards 
            |> List.map(fun (cardRank, cardSuit) -> cardRank) 
            |> List.sortDescending
            |> List.head

    let determineKicker (handRankCards : Card list) : CardRank option =
        if handRankCards.Length > 5 then None else
        let headRank, _  = 
            orderedCards 
            |> List.filter (fun card -> not (List.contains card handRankCards)) 
            |> List.head
        Some headRank

    let createHand' (handRank : HandRank) (handRankValue : CardRank) (handKicker : CardRank option) =
        {
            rank = handRank 
            rankValue = handRankValue
            kicker = handKicker
        }

    let straightCards = determineStraightCards orderedCards
    let flushCards = determineFlushCards orderedCards
    let straightFlushCards = 
        match straightCards with
            | [] -> List.Empty
            | _ -> if isFlush (straightCards) then straightCards else List.Empty
    let royalFlushCards =
        match straightFlushCards with
            | [] -> List.Empty
            | _ ->  if isHighestStraight straightFlushCards then straightFlushCards else List.Empty

    let pokerCards = determineCardsOfAKind KindSize.Quadruple CardAppearance.Once
    let threeOfAKindCards = determineCardsOfAKind KindSize.Triple CardAppearance.Once
    let twoPairsCards = determineCardsOfAKind KindSize.Pair CardAppearance.Twice
    let pairCards = determineCardsOfAKind KindSize.Pair CardAppearance.Once
    let fullHouseCards =
        if threeOfAKindCards.Length > 0 && pairCards.Length > 0 
        then threeOfAKindCards @ pairCards 
        else List.Empty
    let highCards = orderedCards |> List.take 5

    let hand = 
        if isNotEmpty royalFlushCards then createHand' HandRank.RoyalFlush (determineRankValue royalFlushCards) None else
        if isNotEmpty straightFlushCards then createHand' HandRank.StraightFlush (determineRankValue straightFlushCards) None else
        if isNotEmpty pokerCards then createHand' HandRank.Poker (determineRankValue pokerCards) (determineKicker pokerCards) else
        if isNotEmpty fullHouseCards then createHand' HandRank.FullHouse (determineRankValue threeOfAKindCards) None else
        if isNotEmpty flushCards then createHand' HandRank.Flush (determineRankValue flushCards) None else
        if isNotEmpty straightCards then createHand' HandRank.Straight (determineRankValue straightCards) None else
        if isNotEmpty threeOfAKindCards then createHand' HandRank.ThreeOfAKind (determineRankValue threeOfAKindCards) (determineKicker threeOfAKindCards) else
        if isNotEmpty twoPairsCards then createHand' HandRank.TwoPair (determineRankValue twoPairsCards) (determineKicker twoPairsCards) else
        if isNotEmpty pairCards then createHand' HandRank.Pair (determineRankValue pairCards) (determineKicker pairCards) else
        createHand' HandRank.HighCard (determineRankValue highCards) None

    hand

let compareHands : CompareHands = fun hand1 hand2 ->
    let rankComparison = 
        compare hand1.rank hand2.rank

    let rankValueComparison = 
        compare hand1.rankValue hand2.rankValue

    let kickerComparison =
        if hand1.kicker.IsSome && hand2.kicker.IsSome then
            compare hand1.kicker hand2.kicker
        else if hand1.kicker.IsNone && hand2.kicker.IsNone then
            0
        else
            invalidArg "kicker" "Both kickers has to be 'Some' or 'None'"          
        
    if rankComparison <> 0 then 
        rankComparison
    else if rankValueComparison <> 0 then
        rankValueComparison
    else
        kickerComparison