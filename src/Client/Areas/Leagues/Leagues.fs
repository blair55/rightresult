namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open Areas

module Leagues =

  type LeagueList =
    Map<PrivateLeagueId, PlayerLeagueViewModel>

  type Model =
    LeagueList WebData

  type Msg =
    | Init of Result<string, exn>
    | LeaguesReceived of Rresult<LeagueList>
    | NavTo of Route

  let init api player =
    printfn "************* initing"
    Fetching,
      Cmd.OfAsync.either
        api.getPlayerLeagues
        player.Token
        LeaguesReceived
        (fun a -> printfn "************ %A" a; Ok "???" |> Init)
        // (Error >> Init)

  let leaguesList dispatch (model:LeagueList) =
    model
    |> Map.toList
    |> List.map (
      fun (PrivateLeagueId leagueId, { LeagueName = (LeagueName leaguename)}) ->
        Card.card []
          [ Card.content [ Props [ Style [ Padding "0.6em 1em" ] ] ]
              [ a [ OnClick (fun _ -> string leagueId |> LeagueRoute |> LeaguesRoute |> NavTo |> dispatch) ]
                    [ str leaguename ]
              ]
          ])
    |> (fun l ->
      l @ [ Card.card []
              [ Card.footer []
                  [ Card.Footer.a [ Props [ OnClick (fun _ -> LeaguesRoute CreateLeagueRoute |> NavTo |> dispatch) ] ]
                      [ str "Create League"
                      ]
                  ]
              ]
          ])
    |> div []

  let noLeaguesView dispatch =
    Card.card []
      [ Message.message [ Message.Color IsInfo ]
          [ Message.body []
              [ str "You are not in any private leagues."
              ]
          ]
        Card.footer []
          [ Card.Footer.a [ Props [ OnClick (fun _ -> LeaguesRoute CreateLeagueRoute |> NavTo |> dispatch) ] ]
              [ str "Create League"
              ]
          ]
      ]

  let view (model:Model) dispatch =
    div []
      [ Components.pageTitle "Leagues"
        (match model with
        | Success l when Map.isEmpty l -> noLeaguesView dispatch
        | Success l -> leaguesList dispatch l
        | _ ->
          printfn "************* %A" model
          div [] [])
      ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeaguesReceived r ->
      resultToWebData r, []
    | NavTo r ->
      model, Routes.navTo r