module FsFhtw.Tests

open NUnit.Framework
open Domain
open Game

let cardRank =
    fst

let cardSuit =
    snd

let serializeCard (card : Card) =
    (cardRank card |> string) + (cardSuit card |> string)

let serializeDeck (deck : Deck) =
    List.map (fun c -> serializeCard) deck

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


