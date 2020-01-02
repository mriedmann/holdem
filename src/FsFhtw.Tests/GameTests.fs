module FsFhtw.Tests

open NUnit.Framework
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

let CreateHandTestCaseData =
    [
        ("2♥2♦7♠2♣5♣", "6♦2♠", {rank=HandRank.Poker;    rankValue=CardRank.Two;   kicker=Some CardRank.Seven})
        ("2♥3♦Q♠J♣T♣", "9♦8♠", {rank=HandRank.Straight; rankValue=CardRank.Queen;   kicker=None})
        ("2♥2♦5♠6♣3♣", "K♦Q♠", {rank=HandRank.Pair;     rankValue=CardRank.Two;   kicker=Some CardRank.King})
        ("2♥2♦3♠6♣3♣", "7♦8♠", {rank=HandRank.TwoPair;  rankValue=CardRank.Three;   kicker=Some CardRank.Eight})
        ("A♥K♦Q♠J♣9♣", "8♦8♠", {rank=HandRank.Pair;     rankValue=CardRank.Eight; kicker=Some CardRank.Ace})
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
            "Player4"
        );
        ( //Same rank, different rank value
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ace; kicker = Some CardRank.Six}; holeCards = deserializeDeck "A♦6♠"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.TwoPair; rankValue = CardRank.Ten; kicker = Some CardRank.Five}; holeCards = deserializeDeck "9♦5♠"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.Poker; rankValue = CardRank.Jack; kicker = Some CardRank.Four}; holeCards = deserializeDeck "J♦4♠"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Poker; rankValue = CardRank.Three; kicker = Some CardRank.King}; holeCards = deserializeDeck "3♦K♠"},
            "Player3"
        );
        ( //Same rank, Same rank value, different kicker
            {name = "Player1"; position = 1; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ace; kicker = Some CardRank.Six}; holeCards = deserializeDeck "A♦6♠"},
            {name = "Player2"; position = 2; hand = {rank = HandRank.HighCard; rankValue = CardRank.Ten; kicker = Some CardRank.Five}; holeCards = deserializeDeck "9♦5♠"},
            {name = "Player3"; position = 3; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Four}; holeCards = deserializeDeck "J♥4♠"},
            {name = "Player4"; position = 4; hand = {rank = HandRank.Pair; rankValue = CardRank.Jack; kicker = Some CardRank.Queen}; holeCards = deserializeDeck "J♦Q♠"},
            "Player4"
        )
    ] |> List.map (fun (player1 : Player, player2 : Player, player3 : Player, player4 : Player, expectedWinner : string) -> TestCaseData(player1, player2, player3, player4, expectedWinner))

[<TestCaseSource("EvaluateWinnerTestCaseData")>]
let EvaluateWinnerTest (player1 : Player) (player2 : Player) (player3 : Player) (player4 : Player) (expectedWinner : string) =
    let players = [player1; player2; player3; player4]
    let winner = evaluateWinner players
    Assert.AreEqual(winner.name, expectedWinner)