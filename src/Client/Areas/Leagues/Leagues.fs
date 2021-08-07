namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open Areas
open Fable.FontAwesome

module Leagues =

  type LeagueList = Map<PrivateLeagueId, PlayerLeagueViewModel>

  type Model = LeagueList WebData

  type Msg =
    | Init of Result<string, exn>
    | LeaguesReceived of Rresult<LeagueList>
    | NavTo of Route

  let init api player =
    Fetching, Cmd.OfAsync.perform api.getPlayerLeagues player.Token LeaguesReceived

  let createLeagueButton dispatch =
    Button.button
      ([ Button.IsFullWidth
         Button.IsOutlined
         Button.Color IsWarning
         Button.IsLight
         Button.OnClick(fun _ -> NavTo(LeaguesRoute(CreateLeagueRoute)) |> dispatch) ])
      [ str "Create a league"
        Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]

  let globalLeague dispatch =
    div [ Class "block" ] [
      a [ OnClick
            (fun _ ->
              LeaguesRoute GlobalLeagueRoute
              |> NavTo
              |> dispatch) ] [
        str "Global League"
      ]
    ]

  let leaguesList dispatch (model: LeagueList) =
    model
    |> Map.toList
    |> List.map
         (fun (PrivateLeagueId leagueId, { LeagueName = (LeagueName leaguename) }) ->
           div [ Class "block" ] [
             a [ OnClick
                   (fun _ ->
                     string leagueId
                     |> LeagueRoute
                     |> LeaguesRoute
                     |> NavTo
                     |> dispatch) ] [
               str leaguename
             ]
           ])
    |> div []

  let premTables dispatch =
    Box.box' [] [
      div [ Class "block" ] [
        a [ OnClick
              (fun _ ->
                LeaguePremTableRoute "real"
                |> LeaguesRoute
                |> NavTo
                |> dispatch) ] [
          str "Premier League Table"
        ]
      ]
      div [ Class "block" ] [
        a [ OnClick
              (fun _ ->
                LeaguePremTableRoute "predicted"
                |> LeaguesRoute
                |> NavTo
                |> dispatch) ] [
          str "My Predicted Table"
        ]
      ]
    ]

  let getLeaguesView dispatch leagues =
    if Map.isEmpty leagues then
      div [ Class "block" ] [
        Message.message [ Message.Color IsInfo ] [
          Message.body [] [
            str "You are not in any private leagues"
          ]
        ]
      ]
    else
      leaguesList dispatch leagues

  let view (model: Model) dispatch =
    match model with
    | Success l ->
      div [] [
        Components.pageTitle "Leagues"
        premTables dispatch
        Components.subHeading "Prediction Leagues"

        Box.box' [] [
          globalLeague dispatch
          getLeaguesView dispatch l
        ]

        Box.box' [] [
          createLeagueButton dispatch
        ]
      ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeaguesReceived r -> resultToWebData r, []
    | NavTo r -> model, Routes.navTo r
