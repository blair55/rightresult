namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open System
open Shared
open Fulma
open Routes
open Areas

module LeagueHistory =

  type ViewMode =
    | FixtureSets | Months

  type Model =
    { LeagueId : LeagueId
      ViewMode : ViewMode
      LeagueName : LeagueName WebData
      FixtureSets : LeagueHistoryDoc WebData
      Months : LeagueHistoryDoc WebData
    }

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
      LeagueName = match leagueId with | GlobalLeague -> Success Global.leagueName | PrivateLeague _ -> Fetching
      FixtureSets = Fetching
      Months = Fetching
    },
    Cmd.batch
      ( [ Cmd.OfAsync.either
            (api.getLeagueHistoryFixtureSets leagueId)
            player.Token
            FixtureSetsReceived
            (Error >> Init)
          Cmd.OfAsync.either
            (api.getLeagueHistoryMonths leagueId)
            player.Token
            MonthsReceived
            (Error >> Init)
        ] @
        (match leagueId with
        | GlobalLeague -> []
        | PrivateLeague privateLeagueId ->
          [ Cmd.OfAsync.either
              (api.getPrivateLeagueInfo privateLeagueId)
              player.Token
              PrivateLeagueInfoReceived
              (Error >> Init)
          ]
        )
      )

  let leagueView dispatch model (LeagueName leagueName) (fs:LeagueHistoryDoc) (months:LeagueHistoryDoc) =
    let leagueIdStr =
      match model.LeagueId with
      | GlobalLeague -> Global.identifier
      | PrivateLeague (PrivateLeagueId id) -> string id
    let gwTab = [ a [ OnClick (fun _ -> ChangeViewMode FixtureSets |> dispatch) ] [ str "Gameweeks" ] ]
    let mnTab = [ a [ OnClick (fun _ -> ChangeViewMode Months |> dispatch) ] [ str "Months" ] ]
    let getWindowInt = function
      | Full -> bigint 0
      | Week w -> bigint w
      | Month (y, m) -> (DateTime(y, m, 1)).Ticks |> bigint
    let historyRoute = function
      | Full -> PlayerLeaguesRoute
      | Week w -> LeagueHistoryFixtureSetRoute (leagueIdStr, w)
      | Month (y, m) -> LeagueHistoryMonthRoute (leagueIdStr, y, m)
    let matrixRoute = function
      | Week w -> LeagueMatrixRoute (leagueIdStr, w)
      | _ -> PlayerLeaguesRoute
    let matrixDesc = function
      | Week w -> sprintf "Mx %i" w
      | _ -> ""
    let trWithMx (window, { PlayerName = PlayerName name; Points = points; Description = d }) =
      tr []
        [ td []
            [ a [ OnClick (fun _ -> historyRoute window |> LeaguesRoute |> NavTo |> dispatch) ]
                [ str d ]
            ]
          td []
            [ a [ OnClick (fun _ -> matrixRoute window |> LeaguesRoute |> NavTo |> dispatch) ]
                [ str <| matrixDesc window ]
            ]
          td [] [ str name ]
          td [] [ str (string points.Points) ]
        ]
    let trNoMx (window, { PlayerName = PlayerName name; Points = points; Description = d }) =
      tr []
        [ td []
            [ a [ OnClick (fun _ -> historyRoute window |> LeaguesRoute |> NavTo |> dispatch) ]
                [ str d ]
            ]
          td [] [ str name ]
          td [] [ str (string points.Points) ]
        ]
    let tableBody trF =
      Map.toList
      >> List.sortBy (fun (window, _) -> getWindowInt window)
      >> List.map trF
      >> fun rows ->
          Table.table [ Table.IsHoverable; Table.IsFullWidth ]
            [ tbody [] rows
            ]

    div []
      [ Components.pageTitle leagueName
        Components.subHeading "History"
        Card.card []
          (match model.ViewMode with
          | FixtureSets ->
            [ Tabs.tabs []
                [ Tabs.tab [ Tabs.Tab.IsActive true ] gwTab
                  Tabs.tab [ Tabs.Tab.IsActive false ] mnTab
                ]
              tableBody trWithMx fs
            ]
          | Months ->
            [ Tabs.tabs []
                [ Tabs.tab [ Tabs.Tab.IsActive false ] gwTab
                  Tabs.tab [ Tabs.Tab.IsActive true ] mnTab
                ]
              tableBody trNoMx months
            ]
          )
      ]

  let view (model:Model) dispatch =
    match model.LeagueName, model.FixtureSets, model.Months with
    | Success leagueName, Success fs, Success months -> leagueView dispatch model leagueName fs months
    | _ -> div [] [ str "Could not find league history" ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | FixtureSetsReceived r -> { model with FixtureSets = resultToWebData r }, []
    | MonthsReceived r -> { model with Months = resultToWebData r }, []
    | NavTo r -> model, (Routes.navTo r)
    | ChangeViewMode vm -> { model with ViewMode = vm }, []
    | PrivateLeagueInfoReceived r ->
      match resultToWebData r with
      | Success info -> { model with LeagueName = Success info.LeagueName }, []
      | WebError e -> model, alert e
      | _ -> model, []
