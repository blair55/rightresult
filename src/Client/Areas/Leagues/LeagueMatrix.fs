namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Areas
open Shared
open Fulma
open Routes
open Components

module LeagueMatrix =

  type Model =
    { LeagueId : LeagueId
      GameweekNo : GameweekNo
      Matrix : MatrixDoc WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueMatrixReceived of Rresult<MatrixDoc>
    | NavTo of Route

  let init api player gwno leagueId =
    { LeagueId = leagueId
      GameweekNo = gwno
      Matrix = Fetching
    },
      Cmd.OfAsync.either
        (api.getLeagueMatrix leagueId gwno)
        player.Token
        LeagueMatrixReceived
        (Error >> Init)

  let matrixComponent
    (mCols:Map<FixtureId, MatrixFixture>)
    (mRows:Map<PlayerId, MatrixPlayer>)
    (playerClick:PlayerId -> Unit) =

    let playerLink pId (PlayerName playerName) =
      a [ OnClick (fun _ -> playerClick pId) ] [ str playerName ]

    let sortedMatrixCols =
      mCols
      |> Map.toList
      |> List.sortBy (fun (_, { SortOrder = sortOrder; KickOff = (KickOff ko) }) -> sortOrder, ko)

    sortedMatrixCols
    |> List.map (fun (_, { TeamLine = TeamLine (home, away); State = state; KickOff = ko }) ->
      th [] [
          div [ Class "matrix-head-container" ]
            [ div [ Class "matrix-head matrix-head-left" ] [ badge M home ]
              div [ Class "matrix-head matrix-head-right" ] [ badge M away ]
            ]
        ])
    |> fun cols -> ((th [] [])::cols) @ [ th [] [] ]
    |> fun cols ->


    let buildPlayerColumns (predictions:Map<FixtureId, MatrixPrediction>) =
      sortedMatrixCols
      |> List.map (fun (fId, { State = state }) ->
        match state, predictions.TryFind fId with
        | Open, _ -> td [] []
        | KickedOff, Some { Prediction = scoreLine; IsDoubleDown = dd } ->
          td [] [ ScoreBox.kickedOffScoreBox scoreLine dd ]
        | KickedOff, None ->
          td [] [ ScoreBox.emptyScoreBox() ]
        | Classified _, Some { Prediction = scoreLine; IsDoubleDown = dd; Points = p } ->
          match p with
          | Some (_, category) ->
            td [] [ ScoreBox.classifiedScoreBox scoreLine dd category ]
          | _ ->
            td [] [ ScoreBox.kickedOffScoreBox scoreLine dd ]
        | Classified _, None ->
          td [] [ ScoreBox.emptyScoreBox() ]
        )

    mRows
    |> Map.toList
    |> List.sortByDescending (fun (_, { TotalPoints = totalPoints }) -> totalPoints)
    |> List.map (fun (playerId, { PlayerName = playerName; Predictions = predictions; TotalPoints = totalPoints }) ->
      tr [ Class "matrix-player-row" ] ((td [ Class "matrix-player-name" ] [ playerLink playerId playerName ]) :: buildPlayerColumns predictions @ [ td [ Class "matrix-player-score" ] [ str <| sprintf "%i pts" totalPoints ] ]))
    |> fun rws ->

    Table.table [ Table.IsHoverable; Table.IsFullWidth; Table.CustomClass "matrix" ]
      [ thead []
          [ tr [] cols
          ]
        tbody [] rws
      ]

  let matrixView
    { FixtureSetId = fixtureSetId
      LeagueId = leagueId
      LeagueName = (LeagueName leagueName)
      GameweekNo = (GameweekNo gwno)
      Columns = mCols
      Rows = mRows
    } (model:Model) dispatch =
    let playerClick (PlayerId pId) =
      pId |> (PlayerRoute >> PlayersRoute >> NavTo >> dispatch)
    div [ ClassName "block" ]
      [ Components.pageTitle leagueName
        Components.subHeading <| sprintf "Gameweek %i Matrix" gwno
        Card.card []
          [ matrixComponent mCols mRows playerClick
          ]
      ]

  let view (model:Model) dispatch =
    match model.Matrix with
    | NotAsked
    | Fetching -> div [] []
    | WebError _ -> div [] [ str "could not find league" ]
    | Success matrix -> matrixView matrix model dispatch

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueMatrixReceived r -> { model with Matrix = resultToWebData r }, []
    | NavTo r -> model, navTo r
