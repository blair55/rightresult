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
    Fetching,
      Cmd.OfAsync.perform
        api.getPlayerLeagues
        player.Token
        LeaguesReceived

  let createLeagueButton dispatch =
    [ Card.card [ Props [ Style [ Width "100%" ] ] ]
        [ Card.footer []
            [ Card.Footer.a [ ]
                [ Button.button
                    [ Button.IsFullWidth
                      Button.Color IsPrimary
                      Button.OnClick (fun _ -> LeaguesRoute CreateLeagueRoute |> NavTo |> dispatch) ]
                    [ str "Create Private League" ]
                ]
            ]
        ]
    ]

  let globalLeague dispatch =
      Card.card []
        [ Card.content [ Props [ Style [ Padding "0.6em 1em" ] ] ]
            [ a [ OnClick (fun _ -> LeaguesRoute GlobalLeagueRoute |> NavTo |> dispatch) ]
                  [ str "Global League" ]
            ]
        ]

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
    |> fun l -> l @ (createLeagueButton dispatch)
    |> div []

  let noLeaguesView dispatch =
    Components.cardWithFooter
      [ Message.message [ Message.Color IsInfo ]
          [ Message.body []
              [ str "You are not in any private leagues"
              ]
          ]
      ]
      (createLeagueButton dispatch)

  let premTables dispatch =
    div [ Style [MarginBottom "2em" ] ]
      [ Card.card []
          [ Card.content [ Props [ Style [ Padding "0.6em 1em" ] ] ]
              [ a [ OnClick (fun _ -> LeaguePremTableRoute "real" |> LeaguesRoute |> NavTo |> dispatch) ]
                  [ str "Premier League Table" ]
              ]
          ]

        Card.card []
          [ Card.content [ Props [ Style [ Padding "0.6em 1em" ] ] ]
              [ a [ OnClick (fun _ -> LeaguePremTableRoute "predicted" |> LeaguesRoute |> NavTo |> dispatch) ]
                  [ str "My Predicted Table" ]
              ]
          ]
      ]

  let getLeaguesView dispatch leagues =
    if Map.isEmpty leagues
    then noLeaguesView dispatch
    else leaguesList dispatch leagues

  let view (model:Model) dispatch =
    match model with
    | Success l ->
      div []
        [ Components.pageTitle "Leagues"
          premTables dispatch
          Components.subHeading "Prediction Leagues"
          globalLeague dispatch
          getLeaguesView dispatch l
        ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeaguesReceived r ->
      resultToWebData r, []
    | NavTo r ->
      model, Routes.navTo r