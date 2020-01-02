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
        //One pair
        ("2♥2♦5♠6♣3♣", "K♦Q♠", {rank=HandRank.Pair; rankValue=CardRank.Two; kicker=Some CardRank.King})
        //Two pairs
        ("2♥2♦3♠6♣3♣", "7♦8♠", {rank=HandRank.TwoPair; rankValue=CardRank.Three; kicker=Some CardRank.Eight})
        //Three pairs high card within third pair
        ("A♥A♦Q♠Q♣T♣", "T♦8♠", {rank=HandRank.TwoPair; rankValue=CardRank.Ace; kicker=Some CardRank.Ten})
        //Three pairs high card outside third pair
        ("A♥A♦Q♠Q♣3♣", "3♦K♠", {rank=HandRank.TwoPair; rankValue=CardRank.Ace; kicker=Some CardRank.King})
        //Pair
        ("A♥K♦Q♠J♣9♣", "8♦8♠", {rank=HandRank.Pair; rankValue=CardRank.Eight; kicker=Some CardRank.Ace})
        //Three of a kind
        ("K♥K♦K♠J♣T♠", "9♦8♠", {rank=HandRank.ThreeOfAKind; rankValue=CardRank.King; kicker=Some CardRank.Jack})
        //Two times Three of a kind - is FullHouse
        ("K♥K♦K♠J♣J♠", "J♦8♠", {rank=HandRank.FullHouse; rankValue=CardRank.King; kicker=None})
        //Poker
        ("2♥2♦7♠2♣5♣", "6♦2♠", {rank=HandRank.Poker; rankValue=CardRank.Two; kicker=Some CardRank.Seven})
        //Poker and Pair - Only Poker should count
        ("2♥2♦7♠2♣7♣", "6♦2♠", {rank=HandRank.Poker; rankValue=CardRank.Two; kicker=Some CardRank.Seven})
        //Straight
        ("2♥3♦Q♠J♣T♣", "9♦8♠", {rank=HandRank.Straight; rankValue=CardRank.Queen; kicker=None})
        //Royal Flush
        ("A♥K♥Q♥J♥T♥", "8♦8♠", {rank=HandRank.RoyalFlush; rankValue=CardRank.Ace; kicker=None})
        //Flush
        ("3♥K♥5♥J♥T♥", "8♦8♠", {rank=HandRank.Flush; rankValue=CardRank.King; kicker=None})
        //Straight Flush
        ("3♥K♥Q♥J♥T♥", "9♥8♠", {rank=HandRank.StraightFlush; rankValue=CardRank.King; kicker=None})
        //Full House
        ("2♥2♦3♠6♣3♣", "3♦8♠", {rank=HandRank.FullHouse; rankValue=CardRank.Three; kicker=None})
        //Straight and flush in the same deck of cards
        ("K♥Q♦J♥T♥9♣", "2♥7♥", {rank=HandRank.Flush; rankValue=CardRank.King; kicker=None})
        //Three of a kind and flush in the same deck of cards
        ("K♥K♦K♣T♥9♥", "3♥4♥", {rank=HandRank.Flush; rankValue=CardRank.King; kicker=None})
        //Longer straight - higher is non straight flush
        ("A♦K♥Q♥J♥T♥", "9♥8♠", {rank=HandRank.StraightFlush; rankValue=CardRank.King; kicker=None})
        //Straight with Ace as One
        ("A♦2♥3♥4♥7♥", "5♣8♠", {rank=HandRank.Straight; rankValue=CardRank.Five; kicker=None})
        //StraightFlush with Ace as one and second Ace
        ("A♦2♥3♥4♥7♥", "5♣A♥", {rank=HandRank.StraightFlush; rankValue=CardRank.Five; kicker=None})
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
         +1) // First is higher
        ({rank=HandRank.Straight; rankValue=CardRank.Ace;   kicker=None},
         {rank=HandRank.Straight; rankValue=CardRank.Ace;   kicker=None},
         0) // Equals
        ({rank=HandRank.Pair;     rankValue=CardRank.Jack;   kicker=Some CardRank.Seven},  
         {rank=HandRank.TwoPair;     rankValue=CardRank.Two;   kicker=Some CardRank.King},
         -1) // Second is higher
        ({rank=HandRank.TwoPair;  rankValue=CardRank.Five;   kicker=Some CardRank.Two}, 
         {rank=HandRank.TwoPair;  rankValue=CardRank.Two;   kicker=Some CardRank.Eight},
         +1) // First is higher
        ({rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Jack},   
         {rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Ace},
         -1) // Second is higher
    ] |> List.map (fun (q, n, d) -> TestCaseData(q,n,d))

[<TestCaseSource("CompareHandsTestCaseData")>]
let CompareHandsTest 
    (hand1:Hand) 
    (hand2:Hand) 
    (expectedCompareResult:int) =

    let compareResult = compareHands hand1 hand2

    Assert.AreEqual(expectedCompareResult, compareResult)

let EvaluateWinnerTestCaseData =
    [
        ( //Different rank
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ace; kicker = Some CardRank.Six}; holeCards = deserializeDeck "A♦6♠"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.TwoPair; rankValue = CardRank.Ten; kicker = Some CardRank.Five}; holeCards = deserializeDeck "9♦5♠"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.ThreeOfAKind; rankValue = CardRank.Jack; kicker = Some CardRank.Four}; holeCards = deserializeDeck "J♦4♠"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Poker; rankValue = CardRank.Three; kicker = Some CardRank.King}; holeCards = deserializeDeck "3♦K♠"},
            ["Player4"]
        )
        ( //Same rank, different rank value
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ace; kicker = Some CardRank.Six}; holeCards = deserializeDeck "A♦6♠"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.TwoPair; rankValue = CardRank.Ten; kicker = Some CardRank.Five}; holeCards = deserializeDeck "9♦5♠"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.Poker; rankValue = CardRank.Jack; kicker = Some CardRank.Four}; holeCards = deserializeDeck "J♦4♠"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Poker; rankValue = CardRank.Three; kicker = Some CardRank.King}; holeCards = deserializeDeck "3♦K♠"},
            ["Player3"]
        )
        ( //Same rank, Same rank value, different kicker
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ace; kicker = Some CardRank.Six}; holeCards = deserializeDeck "A♦6♠"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ten; kicker = Some CardRank.Five}; holeCards = deserializeDeck "9♦5♠"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Four}; holeCards = deserializeDeck "J♥4♠"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Queen}; holeCards = deserializeDeck "J♦Q♠"},
            ["Player4"]
        )
        ( //Multiple Winners - two winners
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Nine; kicker = Some CardRank.Six}; holeCards = deserializeDeck "7♦6♦"},
            {name = "Player2"; position = 3; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Seven}; holeCards = deserializeDeck "J♥7♥"},
            {name = "Player3"; position = 2; hand = {rank = HandRank.HighCard; rankValue = CardRank.Eight; kicker = Some CardRank.Six}; holeCards = deserializeDeck "7♣6♣"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Seven}; holeCards = deserializeDeck "J♠7♠"},
            ["Player2"; "Player4"]
        )
        ( //Multiple Winners - all four win
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Jack; kicker = Some CardRank.Six}; holeCards = deserializeDeck "J♦6♦"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.HighCard; rankValue = CardRank.Jack; kicker = Some CardRank.Six}; holeCards = deserializeDeck "J♣6♣"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.HighCard; rankValue = CardRank.Jack; kicker = Some CardRank.Six}; holeCards = deserializeDeck "J♥6♥"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.HighCard; rankValue = CardRank.Jack; kicker = Some CardRank.Six}; holeCards = deserializeDeck "J♠6♠"},
            ["Player1"; "Player2"; "Player3"; "Player4"]
        )
    ] |> List.map (fun (player1 : Player, player2 : Player, player3 : Player, player4 : Player, expectedWinner : string list) -> 
        TestCaseData(player1, player2, player3, player4, expectedWinner))

[<TestCaseSource("EvaluateWinnerTestCaseData")>]
let EvaluateWinnerTest (player1 : Player) (player2 : Player) (player3 : Player) (player4 : Player) (expectedWinner : string list) =
    let players = [player1; player2; player3; player4]
    let winner = evaluateWinner players
    Assert.IsTrue( expectedWinner |> List.forall2 (fun player winnerName -> player.name = winnerName) winner )