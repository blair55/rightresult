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

  let boxLinkButton dispatch route icon text =
    Box.box' [ Props [ Style [ MarginBottom "2em" ] ] ] [
      Button.button
        ([ Button.IsFullWidth
           Button.IsOutlined
           Button.Color IsWarning
           Button.IsLight
           Button.OnClick(fun _ -> dispatch route) ])
        [ str text
          span [ Style [ MarginLeft "3px" ] ] [
            Fa.i [ icon ] []
          ] ]
    ]

  let createLeagueButton dispatch leagues =
    div [ Style [ MarginBottom "1em" ] ] [
      (if Map.isEmpty leagues then
         Message.message [ Message.Color IsInfo
                           Message.Modifiers [ Modifier.IsMarginless ] ] [
           Message.body [] [
             str "You are not in any private leagues"
           ]
         ]
       else
         div [] [])
      boxLinkButton dispatch (NavTo(LeaguesRoute(CreateLeagueRoute))) Fa.Solid.AngleDoubleRight "Create a League"
    ]

  let bold s = b [] [ str s ]

  let cashLeagueView dispatch (model: LeagueList) =
    model
    |> Map.toList
    |> List.map fst
    |> List.contains CashLeague.identifier
    |> function
      // | false
      | true ->
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          Panel.panel [ Panel.Color IsPrimary ] [
            Components.panelAnchor
              Fa.Solid.MoneyBillAlt
              "Prediction League 1"
              (NavTo >> dispatch)
              (LeaguesRoute(LeagueRoute(CashLeague.rawId)))
          ]
        ]
      | false ->
        div [] [
          Message.message [ Message.Color IsInfo
                            Message.Modifiers [ Modifier.IsMarginless ] ] [
            Message.body [ Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [
              Content.content [] [
                li [] [ str "£25 entry" ]
                li [] [
                  str "All cash paid in winnings"
                ]
                li [] [
                  str "Monthly payouts & bonus prizes"
                ]
                li [] [
                  str "Email updates & WhatsApp group"
                ]
              ]
            ]
          ]
          boxLinkButton
            dispatch
            (NavTo(LeaguesRoute(JoinLeagueRoute(CashLeague.rawId))))
            Fa.Solid.AngleDoubleRight
            "Join the Cash League"
        ]


  let leaguesList dispatch (model: LeagueList) =
    model
    |> Map.toList
    |> List.filter (fst >> (=) CashLeague.identifier >> not)
    |> List.map (fun (PrivateLeagueId leagueId, { LeagueName = (LeagueName leaguename) }) ->
      Components.panelAnchor Fa.Solid.Trophy leaguename (NavTo >> dispatch) (LeaguesRoute(LeagueRoute(string leagueId))))
    |> Panel.panel [ Panel.Color IsPrimary
                     Panel.Modifiers [ Modifier.IsMarginless ] ]

  // let globalLeaguePanel dispatch =
  //   Panel.panel [ Panel.Color IsPrimary ] [
  //     (Components.panelAnchor Fa.Solid.GlobeAfrica "Global League" (NavTo >> dispatch) (LeaguesRoute(GlobalLeagueRoute)))
  //   ]

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
        Components.pageTitle "Premier League"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "1em" ] ] ] [
          premTables dispatch
        ]
        Components.pageTitle "My Leagues"
        Card.card [ CustomClass "card-footer-only"
                    Props [ Style [ MarginBottom "0em" ] ] ] [
          leaguesList dispatch l
        ]
        createLeagueButton dispatch l
        Components.pageTitle "Cash League (PL1)"
        cashLeagueView dispatch l

      // Components.subHeading "Global League"
      // Card.card [ CustomClass "card-footer-only"
      //             Props [ Style [ MarginBottom "1em" ] ] ] [
      //   globalLeaguePanel dispatch
      // ]
      ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeaguesReceived r -> resultToWebData r, []
    | NavTo r -> model, Routes.navTo r