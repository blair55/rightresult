namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open System
open Shared
open Fulma
open Routes
open Areas
open Components
open Fable.FontAwesome

module LeagueHistory =

  type ViewMode =
    | FixtureSets
    | Months

  type Model =
    { LeagueId: LeagueId
      ViewMode: ViewMode
      LeagueName: LeagueName WebData
      FixtureSets: LeagueHistoryDoc WebData
      Months: LeagueHistoryDoc WebData }

  type Msg =
    | Init of Result<string, exn>
    | FixtureSetsReceived of Rresult<LeagueHistoryDoc>
    | PrivateLeagueInfoReceived of Rresult<PrivateLeagueInfo>
    | MonthsReceived of Rresult<LeagueHistoryDoc>
    | ChangeViewMode of ViewMode
    | NavTo of Route

  let init api player leagueId =
    { LeagueId = leagueId
      ViewMode = FixtureSets
      LeagueName =
        match leagueId with
        | GlobalLeague -> Success Global.leagueName
        | PrivateLeague _ -> Fetching
      FixtureSets = Fetching
      Months = Fetching },
    Cmd.batch (
      [ Cmd.OfAsync.either (api.getLeagueHistoryFixtureSets leagueId) player.Token FixtureSetsReceived (Error >> Init)
        Cmd.OfAsync.either (api.getLeagueHistoryMonths leagueId) player.Token MonthsReceived (Error >> Init) ]
      @ (match leagueId with
         | GlobalLeague -> []
         | PrivateLeague privateLeagueId ->
           [ Cmd.OfAsync.either
               (api.getPrivateLeagueInfo privateLeagueId)
               player.Token
               PrivateLeagueInfoReceived
               (Error >> Init) ])
    )

  let menuLinks
    ({ Model.LeagueId = leagueId }) =
    [ Fa.Solid.Trophy, "League", LeaguesRoute(LeagueRoute(Components.leagueIdStr leagueId)) ]

  let leagueView dispatch model (LeagueName leagueName) (fs: LeagueHistoryDoc) (months: LeagueHistoryDoc) =
    let gwTab =
      [ a [ OnClick(fun _ -> ChangeViewMode FixtureSets |> dispatch) ] [
          str "Gameweeks"
        ] ]

    let mnTab =
      [ a [ OnClick(fun _ -> ChangeViewMode Months |> dispatch) ] [
          str "Months"
        ] ]

    let getWindowInt =
      function
      | Week (GameweekNo w) -> bigint w
      | Month (YearMonth(y, m)) -> (DateTime(y, m, 1)).Ticks |> bigint
      | _ -> bigint 0

    let historyRoute =
      function
      | Week (GameweekNo w) -> LeagueHistoryFixtureSetRoute(leagueIdStr model.LeagueId, w)
      | Month (YearMonth(y, m)) -> LeagueHistoryMonthRoute(leagueIdStr model.LeagueId, y, m)
      | _ -> PlayerLeaguesRoute

    let matrixRoute =
      function
      | Week (GameweekNo w) -> LeagueMatrixRoute(leagueIdStr model.LeagueId, w)
      | _ -> PlayerLeaguesRoute

    let gameweekTr
      (
        window,
        { PlayerName = PlayerName name
          Points = points
          Description = d }
      ) =
      tr [] [
        td [] [
          a (Components.anchorNavProps (NavTo >> dispatch) (LeaguesRoute(historyRoute window))) [ str d ]
        ]
        // td [] [ str "" ] /// todo month
        td [] [ str name ]
        td [] [ str (string points.Points) ]
      ]

    let monthTr
      (
        window,
        { PlayerName = PlayerName name
          Points = points
          Description = d }
      ) =
      tr [] [
        td [] [
          a (Components.anchorNavProps (NavTo >> dispatch) (LeaguesRoute(historyRoute window))) [ str d ]
        ]
        td [] [ str name ]
        td [] [ str (string points.Points) ]
      ]

    let tableBody trF =
      Map.toList
      >> List.sortBy (fun (window, _) -> getWindowInt window)
      >> List.map trF
      >> fun rows ->
           Table.table [ Table.IsHoverable; Table.IsFullWidth ] [
             tbody [] rows
           ]

    div [] [
      Components.pageTitle leagueName
      Components.subHeading "History"
      if Map.isEmpty fs && Map.isEmpty months then
        Message.message [ Message.Color IsWarning ] [
          Message.body [ Modifiers [ Modifier.TextAlignment(Screen.Mobile, TextAlignment.Left) ] ] [
            str (sprintf "There is no history for this league yet. ")
            a
              (Components.anchorNavProps (NavTo >> dispatch) (LeaguesRoute(LeagueRoute(leagueIdStr model.LeagueId))))
              [ str "League home" ]
            str "."
          ]
        ]
      else
        Card.card
          []
          (match model.ViewMode with
           | FixtureSets ->
             [ Tabs.tabs [] [
                 Tabs.tab [ Tabs.Tab.IsActive true ] gwTab
                 Tabs.tab [ Tabs.Tab.IsActive false ] mnTab
               ]
               tableBody gameweekTr fs ]
           | Months ->
             [ Tabs.tabs [] [
                 Tabs.tab [ Tabs.Tab.IsActive false ] gwTab
                 Tabs.tab [ Tabs.Tab.IsActive true ] mnTab
               ]
               tableBody monthTr months ])
        Components.SubMenu.element (NavTo >> dispatch) (menuLinks model)
    ]

  let view (model: Model) dispatch =
    match model.LeagueName, model.FixtureSets, model.Months with
    | Success leagueName, Success fs, Success months -> leagueView dispatch model leagueName fs months
    | Success leagueName, WebError _, WebError _ -> leagueView dispatch model leagueName Map.empty Map.empty
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | FixtureSetsReceived r ->
      { model with
          FixtureSets = resultToWebData r },
      []
    | MonthsReceived r ->
      { model with
          Months = resultToWebData r },
      []
    | NavTo r -> model, (Routes.navTo r)
    | ChangeViewMode vm -> { model with ViewMode = vm }, []
    | PrivateLeagueInfoReceived r ->
      match resultToWebData r with
      | Success info ->
        { model with
            LeagueName = Success info.LeagueName },
        []
      | WebError e -> model, alert e
      | _ -> model, []
