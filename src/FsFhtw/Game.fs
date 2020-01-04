module Game

open Domain 
open Helpers

let createDeck : CreateDeck = fun () ->
    cardSuits 
    |> List.collect (fun suite -> [ for i in 0 .. 12 -> (cardRanks.[i], suite)])

let shuffleDeck : ShuffleDeck = 
    List.toArray >> KnuthShuffle >> Array.toList

let dealCards(deck : ShuffledDeck) (number : int) =
    deck |> List.splitAt (number)

let dealHoleCards : DealHoleCards = fun deck ->
    dealCards deck 2

let dealCommunityCards : DealCommunityCards = fun deck ->
    dealCards deck 5

let determineRankValue (cards) =
    cards 
    |> List.map(cardRank) 
    |> List.sortDescending
    |> List.head

let determineFlushCards (cards) =
    let getCardsOfSuit (cards) (suit : CardSuit) =
        cards |> List.filter (fun (_, cardSuit) -> cardSuit = suit)

    let flushCards = 
        cardSuits 
        |> List.map (getCardsOfSuit cards >> (fun x -> (x, List.length x))) 
        |> List.filter (fun (_, length) -> length >= 5) 
        |> List.map (fst)
    
    if isNotEmpty flushCards then
        Finished (flushCards.Head, HandRank.Flush, determineRankValue flushCards.Head)
    else
        Continue cards

let isFlush =
    determineFlushCards >> hasFinished
        
let determineCardsOfAKind (targetKind: HandRank) (cards) =
    let kindSize, appearance = 
        match targetKind with
        | HandRank.Pair -> (2,1)
        | HandRank.TwoPair -> (2,2)
        | HandRank.ThreeOfAKind -> (3,1)
        | HandRank.Poker -> (4,1)
        | _ -> invalidArg "targetKind" "invalid target for this function"
    
    let filteredCardRankGroups =
        cards
            |> List.groupBy (cardRank) 
            |> List.filter (fun (_, cardList) -> cardList.Length >= int kindSize)
    if filteredCardRankGroups.Length >= int appearance then 
        let setCards = 
            filteredCardRankGroups 
            |> List.collect (fun (_, cardList) -> cardList)
            |> List.sortByDescending (cardRank)
            |> List.take (kindSize * appearance)
        Finished (setCards, targetKind, determineRankValue setCards)
    else 
        Continue cards

let isSpecialCaseAceOne (cards : Card list) : bool =
    let cardRanks = cards |> List.map (fun (cardRank, cardSuit) -> cardRank) 
    List.contains CardRank.Ace cardRanks && List.contains CardRank.Two cardRanks

let determineStraightCards (cards) =
    let distinctCards = 
        cards
        |> List.distinctBy(cardRank)

    let getDistinctRankSortedCards (numberOfCards : int) =
        distinctCards
        |> List.sortBy(cardRank)
        |> List.take numberOfCards
        |> List.sortByDescending (cardRank)
    
    if distinctCards.Length >= 5 then
        
        let hasNextCardExpectedRank (currentCardRank : CardRank) (nextCardRank : CardRank) =  
            if currentCardRank = CardRank.Two && nextCardRank = CardRank.Ace then 
                true
            else
                (int nextCardRank = int currentCardRank - 1)
        
        let areCardsInStraight (cards) =
            cards
            |> List.pairwise
            |> List.forall (fun ((currentCardRank, _), (nextCardRank, _)) -> 
               hasNextCardExpectedRank currentCardRank nextCardRank)
        
        let isSpecialCase : bool = isSpecialCaseAceOne cards

        let bottomToTopsortedCards = 
            if isSpecialCase then
                let aceCard = 
                    List.find (fun (cardRank, _) -> cardRank = CardRank.Ace) cards
                getDistinctRankSortedCards 4 @ [aceCard]
            else
                getDistinctRankSortedCards 5

        let topToBottomsortedCards = 
            distinctCards
            |> List.sortByDescending (cardRank)
            |> List.take 5

        // for more than five cards, checks for a straight has to be done top down and vica versa
        if isSpecialCase && areCardsInStraight bottomToTopsortedCards then
            Finished (bottomToTopsortedCards, HandRank.Straight, CardRank.Five)
        else if areCardsInStraight topToBottomsortedCards then 
            Finished (topToBottomsortedCards, HandRank.Straight, (determineRankValue topToBottomsortedCards))
        else if areCardsInStraight bottomToTopsortedCards then 
            Finished (bottomToTopsortedCards, HandRank.Straight, (determineRankValue bottomToTopsortedCards))
        else
            Continue cards
    else
        Continue cards

let determineStraightFlushCards (cards) =
    let flushCards = determineFlushCards cards
    match flushCards with
    | Finished (handCards, _, _) ->
        let straightFlushCards = determineStraightCards handCards
        match straightFlushCards with
        | Finished (straightFlushHandCards, _, straightFlushHandRankValue) ->
            Finished (straightFlushHandCards, HandRank.StraightFlush, straightFlushHandRankValue)
        | Continue _ -> Continue cards
    | Continue _ -> Continue cards

let determineHighCards (cards) =
    let handCards = cards |> List.take 5
    Finished (handCards, HandRank.HighCard, determineRankValue handCards)

let determinePokerCards = 
    determineCardsOfAKind HandRank.Poker

let determineThreeOfAKindCards = 
    determineCardsOfAKind HandRank.ThreeOfAKind

let determineTwoPairsCards = 
    determineCardsOfAKind HandRank.TwoPair

let determinePairCards = 
    determineCardsOfAKind HandRank.Pair

let determineKicker (handRankCards) (cards) : CardRank option =
    let headRank, _  = 
        cards 
        |> List.except handRankCards
        |> List.head
    Some headRank

let isHighestStraight (cards : Card list) =
    let headRank, _ = cards.Head
    let tailRank, _ = cards |> List.last
    headRank = CardRank.Ace && tailRank = CardRank.Ten

let createHand : CreateHand = fun  (communityCards : CommunityCards) (holeCards : HoleCards) ->  
    let sortedCards = 
        (communityCards @ holeCards) |> List.sortByDescending(cardRank)

    let result = 
        sortedCards 
        |> determineStraightFlushCards
        >>= determineFlushCards
        >>= determineStraightCards
        >>= determinePokerCards
        >>= determineThreeOfAKindCards
        >>= determineTwoPairsCards
        >>= determinePairCards
        >>= determineHighCards
    
    match result with
    | Finished (cards, handRank, handRankValue) -> 
        match handRank with
        // RoyalFlush        
        | HandRank.StraightFlush when isHighestStraight cards -> 
            { rank = HandRank.RoyalFlush; rankValue = handRankValue; kicker = None }
        // StraightFlush, Flush, Straight (no kicker)
        | HandRank.StraightFlush
        | HandRank.Flush
        | HandRank.Straight
        | HandRank.HighCard ->
            { rank = handRank; rankValue = handRankValue; kicker = None }
        // Full House
        | HandRank.ThreeOfAKind when (determinePairCards (sortedCards |> List.except cards) |> hasFinished) ->
            { rank = HandRank.FullHouse; rankValue = handRankValue; kicker = None }
        // Pocker,ThreeOaK,TwoPair,Pair
        | _ -> 
            { rank = handRank; rankValue = handRankValue; kicker = (determineKicker cards sortedCards)}
    | Continue _ -> notImplemented ()

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
            0 //TODO Check for potential issues    
        
    if rankComparison <> 0 then 
        rankComparison
    else if rankValueComparison <> 0 then
        rankValueComparison
    else
        kickerComparison

let evaluateWinner : EvaluateWinner = fun players ->
    let sortedPlayers : Player list = 
        players 
        |> List.sortByDescending(fun player -> player.hand.rank, player.hand.rankValue, player.hand.kicker) 
    let bestPlayerHand = sortedPlayers.Item(0).hand
    List.filter(fun player -> (compareHands player.hand bestPlayerHand) = 0) sortedPlayers
