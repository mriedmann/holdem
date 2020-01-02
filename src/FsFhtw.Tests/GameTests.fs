module FsFhtw.Tests

open NUnit.Framework
open Helpers
open Domain
open DomainSerializer
open Game

[<SetUp>]
let Setup () =
    ()

[<Test>]
let CreateDeckTest1 () =
    let deck = createDeck ()
    Assert.That(deck.Length, Is.EqualTo(52))

[<Test>]
let CreateDeckTest2 () =
    let deck = createDeck ()
    let groups = deck |> List.groupBy cardSuit
    Assert.That(groups.Length, Is.EqualTo(4))
 
[<Test>]
let CreateDeckTest3 () =
    let deck = createDeck ()
    let groups = deck |> List.filter (fun c -> (cardSuit c) = CardSuit.Club) 
    Assert.That(groups.Length, Is.EqualTo(13))

[<Test>]
let ShuffleDeckTest1 () =
    Helpers.SeedRandom 1
    let deck = createDeck ()
    let shuffledDeck = shuffleDeck deck
    Assert.That((shuffledDeck |> serializeDeck), Is.Not.EqualTo((deck |> serializeDeck)))

[<Test>]
let DealHoleCardsTest1 () =
    let deck = "2♥2♦2♠2♣4♣" |> deserializeDeck
    let holeCards, remainingDeck = deck |> dealHoleCards
    Assert.AreEqual(3, remainingDeck.Length)
    Assert.AreEqual(2, holeCards.Length)
    Assert.AreEqual("2♥2♦", (holeCards |> serializeDeck))

[<Test>]
let DealCommunityCardsTest1 () =
    let deck = "2♥2♦J♦2♠2♣4♣3♥3♦3♠7♣8♣" |> deserializeDeck
    let communityCards, remainingDeck = deck |> dealCommunityCards
    Assert.AreEqual(6, remainingDeck.Length)
    Assert.AreEqual(5, communityCards.Length)
    Assert.AreEqual("2♥2♦J♦2♠2♣", (communityCards |> serializeDeck))

[<Test>]
let determineFlushCardsTestPositive () =
    let cards = "2♥3♥J♦4♥6♥8♥3♦" |> deserializeDeck
    let result = determineFlushCards cards
    match result with
    | Finished (_, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.Flush, handRank)
        Assert.AreEqual(CardRank.Eight, handRankValue)
    | Continue _ -> Assert.Fail () 

[<Test>]
let determineFlushCardsTestNegative () =
    let cards = "2♣3♥J♦4♥6♣8♥3♦" |> deserializeDeck
    let result = determineFlushCards cards
    match result with
    | Finished _ -> 
        Assert.Fail ()
    | Continue _ -> 
        Assert.Pass () 

[<Test>]
let determineStraightCardsTestNegative () =
    let cards = "2♣3♥J♦4♥6♣8♥3♦" |> deserializeDeck
    let result = determineStraightCards cards
    match result with
    | Finished _ -> 
        Assert.Fail ()
    | Continue _ -> 
        Assert.Pass ()

[<Test>]
let determineStraightCardsTestPositive () =
    let cards = "2♥3♦8♠9♣T♣J♦Q♠" |> deserializeDeck
    let result = determineStraightCards cards
    match result with
    | Finished (handCards, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.Straight, handRank)
        Assert.AreEqual(CardRank.Queen, handRankValue)
        Assert.AreEqual("Q♠J♦T♣9♣8♠", (handCards |> serializeDeck))
    | Continue _ -> Assert.Fail ()

[<Test>]
let determinePairCardsTestPositive () =
    let cards = "2♥3♦8♠T♣T♣J♦Q♠" |> deserializeDeck
    let result = determinePairCards cards
    match result with
    | Finished (_, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.Pair, handRank)
        Assert.AreEqual(CardRank.Ten, handRankValue)
    | Continue _ -> Assert.Fail ()

[<Test>]
let determineTwoPairsCardsTestPositive () =
    let cards = "2♥3♦8♠T♣T♣Q♦Q♠" |> deserializeDeck
    let result = determineTwoPairsCards cards
    match result with
    | Finished (_, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.TwoPair, handRank)
        Assert.AreEqual(CardRank.Queen, handRankValue)
    | Continue _ -> Assert.Fail ()

[<Test>]
let determineThreeOfAKindCardsTestPositive () =
    let cards = "2♥3♦T♠T♣T♣J♦Q♠" |> deserializeDeck
    let result = determineThreeOfAKindCards cards
    match result with
    | Finished (handCards, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.ThreeOfAKind, handRank)
        Assert.AreEqual(CardRank.Ten, handRankValue)
        Assert.AreEqual("T♠T♣T♣", (handCards |> serializeDeck))
    | Continue _ -> Assert.Fail ()

[<Test>]
let determinePokerCardsTestPositive () =
    let cards = "2♥T♦T♠T♣T♥J♦Q♠" |> deserializeDeck
    let result = determinePokerCards cards
    match result with
    | Finished (_, handRank, handRankValue) -> 
        Assert.AreEqual(HandRank.Poker, handRank)
        Assert.AreEqual(CardRank.Ten, handRankValue)
    | Continue _ -> Assert.Fail ()

let CreateHandTestCaseData =
    [
        ("2♥2♦7♠2♣5♣", "6♦2♠", {rank=HandRank.Poker;    rankValue=CardRank.Two;   kicker=Some CardRank.Seven})
        ("2♥3♦Q♠J♣T♣", "9♦8♠", {rank=HandRank.Straight; rankValue=CardRank.Queen;   kicker=None})
        ("2♥2♦5♠6♣3♣", "K♦Q♠", {rank=HandRank.Pair;     rankValue=CardRank.Two;   kicker=Some CardRank.King})
        ("2♥2♦3♠6♣3♣", "7♦8♠", {rank=HandRank.TwoPair;  rankValue=CardRank.Three;   kicker=Some CardRank.Eight})
        ("A♥K♦Q♠J♣9♣", "8♦8♠", {rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Ace})
        ("A♥K♥Q♥J♥T♥", "8♦8♠", {rank=HandRank.RoyalFlush; rankValue=CardRank.Ace; kicker=None})
        ("3♥K♥5♥J♥T♥", "8♦8♠", {rank=HandRank.Flush; rankValue=CardRank.King; kicker=None})
        ("3♥K♥Q♥J♥T♥", "9♥8♠", {rank=HandRank.StraightFlush; rankValue=CardRank.King; kicker=None})
        ("2♥2♦3♠6♣3♣", "3♦8♠", {rank=HandRank.FullHouse;  rankValue=CardRank.Three;   kicker=None})
    ] |> List.map (fun (q, n, d) -> TestCaseData(q,n,d))

[<TestCaseSource("CreateHandTestCaseData")>]
let CreateHandTest (cc:string) (hc:string) (expectedHand:Hand) =
    let communityCards:CommunityCards = cc |> deserializeDeck
    let holeCards:HoleCards =  hc |> deserializeDeck

    let hand = createHand communityCards holeCards

    Assert.AreEqual(expectedHand.rank, hand.rank)
    Assert.AreEqual(expectedHand.rankValue, hand.rankValue)
    Assert.AreEqual(expectedHand.kicker, hand.kicker)

let CompareHandsTestCaseData =
    [
        ({rank=HandRank.Poker;    rankValue=CardRank.Two;   kicker=Some CardRank.Three}, 
         {rank=HandRank.TwoPair;    rankValue=CardRank.Jack;   kicker=Some CardRank.Queen},
         -1) // First is higher
        ({rank=HandRank.Straight; rankValue=CardRank.Ace;   kicker=None},
         {rank=HandRank.Straight; rankValue=CardRank.Ace;   kicker=None},
         0) // Equals
        ({rank=HandRank.Pair;     rankValue=CardRank.Jack;   kicker=Some CardRank.Seven},  
         {rank=HandRank.TwoPair;     rankValue=CardRank.Two;   kicker=Some CardRank.King},
         +1) // Second is higher
        ({rank=HandRank.TwoPair;  rankValue=CardRank.Five;   kicker=Some CardRank.Two}, 
         {rank=HandRank.TwoPair;  rankValue=CardRank.Two;   kicker=Some CardRank.Eight},
         -1) // First is higher
        ({rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Jack},   
         {rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Ace},
         +1) // Second is higher
    ] |> List.map (fun (q, n, d) -> TestCaseData(q,n,d))

[<TestCaseSource("CompareHandsTestCaseData")>]
[<Ignore("not implemented")>]
let CompareHandsTest 
    (hand1:Hand) 
    (hand2:Hand) 
    (expectedCompareResult:int) =

    let compareResult = compareHands hand1 hand2

    Assert.AreEqual(expectedCompareResult, compareResult)
