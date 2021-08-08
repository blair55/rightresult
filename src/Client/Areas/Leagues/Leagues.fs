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

  let createLeagueButton dispatch leagues =
    Box.box' [] [
      (if Map.isEmpty leagues then
        Message.message [ Message.Color IsInfo ] [
          Message.body [] [
            str "You are not in any private leagues"
          ]
        ] else div [] []
      )

      Button.button
        ([ Button.IsFullWidth
           Button.IsOutlined
           Button.Color IsWarning
           Button.IsLight
           Button.OnClick(fun _ -> NavTo(LeaguesRoute(CreateLeagueRoute)) |> dispatch) ])
        [ str "Create a league"
          span [ Style [ MarginLeft "3px" ] ] [
            Fa.i [ Fa.Solid.AngleDoubleRight ] []
          ] ]
    ]

  let leaguesList dispatch (model: LeagueList) =
    model
    |> Map.toList
    |> List.map
         (fun (PrivateLeagueId leagueId, { LeagueName = (LeagueName leaguename) }) ->
            Panel.Block.div [] [
              Panel.icon [] [
                Fa.i [ Fa.Solid.Trophy ] []
              ]
              a [ OnClick
                    (fun _ ->
                     string leagueId
                     |> LeagueRoute
                     |> LeaguesRoute
                     |> NavTo
                     |> dispatch) ] [
                str leaguename
              ]
            ]
           )
    |> fun items ->
      (Panel.Block.div [] [
        Panel.icon [] [ Fa.i [ Fa.Solid.GlobeEurope ] [] ]
        a [ OnClick
          (fun _ ->
            LeaguesRoute GlobalLeagueRoute
            |> NavTo
            |> dispatch) ] [
          str "Global League"
        ]
      ]::items)
    |> Panel.panel [ Panel.Color IsPrimary ]

  let premTables dispatch =
    Panel.panel [ Panel.Color IsPrimary ] [
      Panel.Block.div [] [
        Panel.icon [] [
          Fa.i [ Fa.Solid.Trophy ] []
        ]
        a [ OnClick
              (fun _ ->
                LeaguePremTableRoute "real"
                |> LeaguesRoute
                |> NavTo
                |> dispatch) ] [
          str "Premier League Table"
        ]
      ]
      Panel.Block.a [] [
        Panel.icon [] [
          Fa.i [ Fa.Solid.UserCircle ] []
        ]
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

  let view (model: Model) dispatch =
    match model with
    | Success l ->
      div [] [
        Components.pageTitle "Prediction Leagues"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          leaguesList dispatch l
        ]
        createLeagueButton dispatch l
        Components.subHeading "Premier League"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          premTables dispatch
        ]
      ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeaguesReceived r -> resultToWebData r, []
    | NavTo r -> model, Routes.navTo r
