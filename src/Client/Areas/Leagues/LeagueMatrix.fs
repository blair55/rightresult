namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Areas
open Shared
open Fulma
open Routes
open Components

module LeagueMatrix =

  type Model =
    { LeagueId: LeagueId
      GameweekNo: GameweekNo
      Matrix: MatrixDoc WebData }

  type Msg =
    | Init of Result<string, exn>
    | LeagueMatrixReceived of Rresult<MatrixDoc>
    | NavTo of Route

  let init api player gwno leagueId =
    { LeagueId = leagueId
      GameweekNo = gwno
      Matrix = Fetching },
    Cmd.OfAsync.either (api.getLeagueMatrix leagueId gwno) player.Token LeagueMatrixReceived (Error >> Init)

  let matrixComponent (mCols: Map<FixtureId, MatrixFixture>) (mRows: Map<PlayerId, MatrixPlayer>) dispatch =

    let playerLink (PlayerId pId) (PlayerName playerName) =
      a (Components.anchorNavProps (NavTo >> dispatch) (PlayersRoute(PlayerRoute pId))) [ str playerName ]

    let sortedMatrixCols =
      mCols
      |> Map.toList
      |> List.sortBy (fun (_, { SortOrder = sortOrder; KickOff = ko }) -> sortOrder, ko.Raw)

    sortedMatrixCols
    |> List.map
         (fun (_,
               { TeamLine = TeamLine (home, away)
                 State = state }) ->
           th [] [
             div [ Class "matrix-head-container" ] [
               div [ Class "matrix-head matrix-head-left" ] [
                 badge M home
               ]
               div [ Class "matrix-head matrix-head-right" ] [
                 badge M away
               ]
             ]
             Components.PredictionScore.ResultScore.element state
           ])
    |> fun cols -> [ th [] [] ] @ cols @ [ th [] [] ]
    |> fun cols ->

         let buildPlayerColumns (predictions: Map<FixtureId, MatrixPrediction>) =
           sortedMatrixCols
           |> List.map
                (fun (fId, { State = state }) ->
                  match state, predictions.TryFind fId with
                  | FixtureState.Open _,
                    Some { Prediction = scoreLine
                           Modifier = PredictionModifier.BigUp as modifier
                           Points = pts } ->
                    td [] [ Components.PredictionScore.element (Some (scoreLine, modifier)) pts ]
                  | FixtureState.Open _, _ ->
                    td [] [ Components.PredictionScore.element None 0 ]
                  | FixtureState.InPlay _,
                    Some { Prediction = scoreLine
                           Modifier = modifier
                           Points = pts } ->
                    td [] [ Components.PredictionScore.element (Some (scoreLine, modifier)) pts ]
                  | FixtureState.InPlay _, None -> td [] [ Components.PredictionScore.element None 0 ]
                  | FixtureState.Classified _,
                    Some { Prediction = scoreLine
                           Modifier = modifier
                           Points = pts } ->
                    td [] [ Components.PredictionScore.element (Some (scoreLine, modifier)) pts ]
                  | FixtureState.Classified _, None ->
                    td [] [ Components.PredictionScore.element None 0 ])

         mRows
         |> Map.toList
         |> List.sortByDescending (fun (_, { TotalPoints = totalPoints }) -> totalPoints)
         |> List.map
              (fun (playerId,
                    { PlayerName = playerName
                      Predictions = predictions
                      TotalPoints = totalPoints }) ->
                tr
                  [ Class "matrix-player-row" ]
                  ((th [ Class "matrix-player-name" ] [
                      playerLink playerId playerName
                    ])
                   :: buildPlayerColumns predictions
                   @ [ td [ Class "matrix-player-score" ] [
                         str <| sprintf "%i pts" totalPoints
                       ] ]))
         |> fun rws ->
              div [Class "matrix"] [
                Table.table [ Table.IsHoverable
                              // Table.IsFullWidth
                              // Table.CustomClass "matrix"
                              Table.Props [ Style [ MarginBottom "1em" ] ] ] [
                  thead [] [ tr [] cols ]
                  tbody [] rws
                ]
              ]

  let menuLinks
    ({ MatrixDoc.LeagueId = leagueId
       GameweekNo = GameweekNo gwno }) =
    [ Fa.Solid.ThList, $"Gameweek {gwno} Table", LeaguesRoute(LeagueHistoryFixtureSetRoute(Components.leagueIdStr leagueId, gwno))
      Fa.Solid.History, "History", LeaguesRoute(LeagueHistoryRoute(Components.leagueIdStr leagueId))
      Fa.Solid.Trophy, "League", LeaguesRoute(LeagueRoute(Components.leagueIdStr leagueId)) ]

  let matrixView
    ({ LeagueName = LeagueName leagueName
       GameweekNo = GameweekNo gwno
       Columns = mCols
       Rows = mRows } as mdoc)
    (model: Model)
    dispatch
    =
    div [] [
      Components.pageTitle leagueName
      Components.subHeading <| sprintf "Gameweek %i Matrix" gwno
      matrixComponent mCols mRows dispatch
      Components.SubMenu.element (NavTo >> dispatch) (menuLinks mdoc)
    ]

  let view (model: Model) dispatch =
    match model.Matrix with
    | NotAsked
    | Fetching -> div [] []
    | WebError _ -> div [] [ str "could not find league" ]
    | Success matrix -> matrixView matrix model dispatch

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueMatrixReceived r ->
      { model with
          Matrix = resultToWebData r },
      []
    | NavTo r -> model, navTo r
