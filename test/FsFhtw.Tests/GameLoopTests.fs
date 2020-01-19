module GameLoopTests

open FsCheck
open FsCheck.NUnit
open Domain

[<Property>]
let ``sending SetName changes the model name`` (msg:string) =
    //given
    let initialState = init ()

    //when
    let actual = 
        initialState 
        |> GameLoop.update (SetName msg)

    //then
    actual = {name = msg; coins = 1000}
   

[<Property>]
let ``sending StartGame changes the coins amount`` (playerNum:int) =
    let prop pn =
        //given
        let initialState = init ()

        //when
        let actual = 
            initialState 
            |> GameLoop.update (StartGame pn)

        //then
        actual.coins <> initialState.coins
    prop playerNum
    

[<Property>]
let ``sending ResetCoin sets the coins to the given value`` () =
    let prop (coins) =
        //given
        let initialState:State = init ()

        //when
        let actual = 
            initialState 
            |> GameLoop.update (ResetCoins coins)

        //then
        actual.coins = coins
    
    Check.QuickThrowOnFailure prop