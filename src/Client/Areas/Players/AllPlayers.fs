namespace Areas.Players

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Fable
open Routes
open Areas

module AllPlayers =

  type Model =
    { Players : (PlayerViewModel list) WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | PlayersReceived of Rresult<PlayerViewModel list>
    | NavTo of Route

  let init api p =
    { Players = Fetching
    },
      Cmd.OfAsync.either
        api.getAllPlayers
        p.Token
        PlayersReceived
        (Error >> Init)

  let playerBlock dispatch { PlayerViewModel.Id = (PlayerId playerId); Name = (PlayerName name) } =
    Card.card []
      [ Card.content [ Props [ Style [ Padding "0.6em 1em" ] ] ]
          [ a [ OnClick (fun _ -> PlayerRoute playerId |> PlayersRoute |> NavTo |> dispatch) ]
                [ str name ]
          ]
      ]

  let playersView (players:PlayerViewModel list) dispatch =
    div []
      (players |> List.map (playerBlock dispatch))

  let fullView dispatch (players:PlayerViewModel list) =
    div []
      [ Components.pageTitle "Player Index"
        playersView players dispatch
      ]

  let view (model:Model) dispatch =
    match model.Players with
    | Success players -> fullView dispatch players
    | _ ->
      div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | PlayersReceived r -> { model with Players = resultToWebData r }, []
    | NavTo r -> model, (Routes.navTo r)
