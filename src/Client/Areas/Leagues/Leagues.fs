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
    div [] [
      (if Map.isEmpty leagues then
         Message.message [ Message.Color IsInfo ] [
           Message.body [] [
             str "You are not in any private leagues"
           ]
         ]
       else
         div [] [])
      Box.box' [ Props [ Style [ MarginBottom "2em" ] ] ] [
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
    ]

  let leaguesList dispatch (model: LeagueList) =
    model
    |> Map.toList
    |> List.map
         (fun (PrivateLeagueId leagueId, { LeagueName = (LeagueName leaguename) }) ->
           Components.panelAnchor
             Fa.Solid.Trophy
             leaguename
             (NavTo >> dispatch)
             (LeaguesRoute(LeagueRoute(string leagueId))))
    |> Panel.panel [ Panel.Color IsPrimary ]

  let globalLeaguePanel dispatch =
    Panel.panel [ Panel.Color IsPrimary ] [
      (Components.panelAnchor Fa.Solid.GlobeAfrica "Global League" (NavTo >> dispatch) (LeaguesRoute(GlobalLeagueRoute)))
    ]

  let premTables dispatch =
    Panel.panel [ Panel.Color IsPrimary ] [
      Components.panelAnchor
        Fa.Solid.Trophy
        "Premier League Table"
        (NavTo >> dispatch)
        (LeaguesRoute(LeaguePremTableRoute "real"))
      Components.panelAnchor
        Fa.Solid.UserCircle
        "My Predicted Table"
        (NavTo >> dispatch)
        (LeaguesRoute(LeaguePremTableRoute "predicted"))
    ]

  let view (model: Model) dispatch =
    match model with
    | Success l ->
      div [] [
        Components.pageTitle "Prediction Leagues"
        Components.subHeading "Global League"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          globalLeaguePanel dispatch
        ]
        Components.subHeading "Private Leagues"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          leaguesList dispatch l
        ]
        createLeagueButton dispatch l
        Components.pageTitle "Premier League"
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
