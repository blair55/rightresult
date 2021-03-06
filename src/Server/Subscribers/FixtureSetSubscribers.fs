﻿namespace Server.Subscribers

open FSharp.Core
open Shared
open Server
open Server.Infrastructure
open Persistence

module FixtureSubscribersAssistance =

  let leagueIdsAndNames deps =
    deps.Queries.getPrivateLeagues ()
    |> List.ofSeq
    |> List.map (fun l -> PrivateLeague l.PrivateLeagueId, l.LeagueName)
    |> fun l -> (GlobalLeague, Global.leagueName)::l

  let getPrivateLeaguesAndLeagueMembers deps =
    deps.Queries.getPrivateLeaguesAndMembers ()
    |> List.ofSeq
    |> List.map (fun (league, members) ->
      PrivateLeague league.PrivateLeagueId,
      league.LeagueName,
      members
      |> List.ofSeq
      |> List.map (fun m -> m.Id))

  let allLeaguesAndMembers deps =
    List.map (fun (p:PlayerRecord) -> p.Id)
    >> fun allPlayerIds -> (GlobalLeague, Global.leagueName, allPlayerIds) :: getPrivateLeaguesAndLeagueMembers deps

  let getPlayerPushSubscriptions (deps:Dependencies) : List<PlayerId * PushSubscription> =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo -> repo.Read PlayerPushSubscriptions
    |> Option.defaultValue []
    |> List.distinct

  let buildColumn (deps:Dependencies) team =
    let getFormGuide team : FormFixture list =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
      repo.Read (FormGuideDocument team)
      |> Option.defaultValue []
      |> List.sortByDescending (fun f -> f.KickOff)
    let getPremTableRow team =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
      repo.Read RealPremTable
      |> Option.bind (fun (table:PremTable) -> Map.tryFind team table.Rows)
      |> Option.defaultValue PremTableRow.Init
    { FixtureDetailsColumn.Team = team
      PremTableRow = getPremTableRow team
      FormGuide = getFormGuide team }

module FixtureSetCreatedSubscribers =

  open Infrastructure.PushNotifications
  open FixtureSubscribersAssistance

  let createFixtureSet (deps:Dependencies) created (FixtureSetId fsId, GameweekNo gwno, fixtures:FixtureRecord list) =
    fixtures
    |> List.minBy (fun { KickOff = KickOff ko } -> ko)
    |> fun { KickOff = KickOff minKo } ->
    { FixtureSetNode.Id = string fsId
      GameweekNo = gwno
      Year = minKo.DateTime.Year
      Month = minKo.DateTime.Month
      Created = created
      IsConcluded = false }
    |> deps.NonQueries.createFixtureSet

    fixtures
    |> List.sortBy (fun { KickOff = KickOff ko } -> ko)
    |> List.mapi (
      fun i { FixtureRecord.Id = FixtureId fId
              GameweekNo = GameweekNo gwno
              KickOff = KickOff ko
              TeamLine = TeamLine (Team home, Team away) } ->
      { FixtureNode.Id = string fId
        FixtureSetId = string fsId
        Created = created
        GameweekNo = gwno
        SortOrder = i
        KickOff = ko
        HomeTeam = home
        AwayTeam = away
        HasKickedOff = false
        HasResult = false
        HomeScore = 0
        AwayScore = 0 })
    |> List.iter (deps.NonQueries.createFixture (FixtureSetId fsId))

  let createMatrix (deps:Dependencies) created (fsId, gwno, fixtures:FixtureRecord list) =
    let columns =
      fixtures
      |> List.map (fun f ->
        f.Id,
        { MatrixFixture.TeamLine = f.TeamLine
          KickOff = f.KickOff
          State = MatrixFixtureState.Open
          SortOrder = f.SortOrder
        })
      |> Map.ofList
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
      FixtureSubscribersAssistance.leagueIdsAndNames deps
      |> List.iter (fun (leagueId, leagueName) ->
        { FixtureSetId = fsId
          LeagueName = leagueName
          LeagueId = leagueId
          GameweekNo = gwno
          Columns = columns
          Rows = Map.empty
        }
        |> repo.Insert (Matrix (leagueId, gwno))
      )

  let notifyPlayers (deps:Dependencies) created (fsId, GameweekNo gwno, _) =
    { PushMessage.Title = sprintf "GW %i fixtures added" gwno
      Body = "Get your predictions in!" }
    |> fun m ->
    FixtureSubscribersAssistance.getPlayerPushSubscriptions deps
    |> List.iter (fun (_, ps) -> deps.PushNotify m ps)

  let createFixtureDetails (deps:Dependencies) _ (_, _, fixtures) =
    fixtures
    |> List.iter (fun { FixtureRecord.Id = fId; KickOff = ko; TeamLine = TeamLine (home, away) } ->
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
        repo.Insert (FixtureDetailsDocument fId)
          { Id = fId; KickOff = ko; Home = buildColumn deps home; Away = buildColumn deps away })

  let all =
    [ createFixtureSet
      createMatrix
      createFixtureDetails
      notifyPlayers
    ]

module FixtureSetConcludedSubscribers =

  let concludeFixtureSet (deps:Dependencies) _ (fsId, _) =
    deps.NonQueries.concludeFixtureSet fsId

  let calculateGlobalGameweekWinner (deps:Dependencies) created (fsId, GameweekNo gwno) =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    LeagueTableDocument (GlobalLeague, Week gwno)
    |> repo.Read
    |> Option.bind (fun table -> table.Members |> List.tryHead)
    |> Option.map (fun (playerId, m) ->
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
        repo.Insert
          GlobalGameweekWinner
          { GlobalGameweekWinner.PlayerId = playerId
            GameweekNo = GameweekNo gwno
            Member = m })
    |> ignore

  let all =
    [ concludeFixtureSet
      calculateGlobalGameweekWinner
    ]

module FixtureKoEditedSubscribers =

  let editFixtureKo (deps:Dependencies) _ (_, fId, ko) =
    deps.NonQueries.editFixtureKo (fId, ko)

  let all =
    [ editFixtureKo
    ]

module FixtureKickedOffSubscribers =

  let kickOffFixture (deps:Dependencies) _ (_, fId) =
    deps.NonQueries.kickOffFixture fId

  let updateMatrix (deps:Dependencies) _ (fsId, fId) =

    let q =
      deps.Queries

    let allPlayers =
      q.getAllPlayers ()

    let playerNameMap =
      allPlayers
      |> List.map (fun p -> p.Id, p.Name)
      |> Map.ofList

    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    q.getFixtureSetGameweekNo fsId
    |> fun gwno ->
    FixtureSubscribersAssistance.allLeaguesAndMembers deps allPlayers
    |> List.iter (fun (leagueId, _, members) ->
      (fun (m:MatrixDoc) ->
        { m with
            Columns =
              m.Columns.Add(fId, { m.Columns.[fId] with State = MatrixFixtureState.KickedOff })
            Rows =
              members
              |> List.map (fun pId ->
                q.getPlayerPredictionForFixture pId fId
                |> Option.map (fun p ->
                  { MatrixPrediction.Prediction = p.ScoreLine
                    IsDoubleDown = p.IsDoubleDown
                    Points = None })
                |> fun mPrediction ->
                m.Rows.TryFind pId
                |> fun mPlayer ->
                match mPlayer, mPrediction with
                | Some pl, Some pr ->
                  pId, { pl with Predictions = pl.Predictions.Add(fId, pr) }
                | Some pl, None ->
                  pId, pl
                | None, Some pr ->
                  pId, { MatrixPlayer.PlayerName = playerNameMap.[pId]; Predictions = [ fId, pr ] |> Map.ofList; TotalPoints = 0 }
                | _ ->
                  pId, { MatrixPlayer.PlayerName = playerNameMap.[pId]; Predictions = Map.empty; TotalPoints = 0 }
                )
                |> Map.ofList
          })
        |> repo.Edit (Matrix (leagueId, gwno))
        |> ignore
      )

  let updatePredictedPremTableForAllPlayers deps _ (_, _) =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    deps.Queries.getAllPlayers ()
    |> List.ofSeq
    |> List.map (fun player ->
      player,
      deps.Queries.getPredictionsForPlayer player.Id
      |> List.ofSeq
      |> List.map (fun (f, p) -> f.TeamLine, p.ScoreLine))
    |> List.iter (fun (player, predictions) ->
      predictions
      |> Points.buildTable PremTable.Init
      |> repo.Insert (PredictedPremTable player.Id))


  let all =
    [ kickOffFixture
      updateMatrix
      updatePredictedPremTableForAllPlayers
    ]

module FixtureClassifiedSubscribers =

  open Points
  open FixtureSubscribersAssistance

  let fixturePredictionToPoints (f:FixtureRecord, p:PredictionRecord) =
    match f.ScoreLine with
    | Some result ->
      Some (p.ScoreLine, p.IsDoubleDown)
      |> Points.getPointsForPrediction result
      |> fst
    | _ ->
      PredictionPointsMonoid.Init

  type PositionNumber = PositionNumber of int
  type PositionCollection = PositionCollection of (PlayerId * LeagueTableMember) list

  let sort (ppm:PredictionPointsMonoid) =
    ppm.Points,
    ppm.CorrectScores + ppm.DoubleDownCorrectScores,
    ppm.CorrectResults + ppm.DoubleDownCorrectResults

  let standingAlgo =
    List.sortByDescending (fun (_, m:LeagueTableMember) -> sort m.Points)
    >> List.groupBy (fun (_, m) -> sort m.Points)
    >> List.map (fun (_, col) -> col.Length, PositionCollection col)
    >> List.fold (fun (totalCount, accPlayers) (_, PositionCollection players) ->
      totalCount+players.Length, (PositionNumber totalCount, PositionCollection players)::accPlayers)
      (1, [])
    >> fun (_, pc) -> pc |> List.map (fun (PositionNumber n, PositionCollection players) -> players |> List.map (fun (pId, mbr) -> pId, { mbr with Position = n }))
    >> List.collect id
    >> List.rev

  let movementAlgo (previousTableMap: Map<PlayerId, LeagueTableMember> option) =
    List.map (fun (playerId, m:LeagueTableMember) ->
      previousTableMap
      |> Option.bind (Map.tryFind playerId)
      |> Option.map (fun prev -> { m with Movement = prev.Position - m.Position })
      |> fun updatedMember -> playerId, Option.defaultValue m updatedMember)

  let updateAllLeagueTables (deps:Dependencies) _ (FixtureSetId fsId, _, _) =

    let q =
      deps.Queries

    let (GameweekNo gwno) =
      q.getFixtureSetGameweekNo (FixtureSetId fsId)

    let (year, month) =
      q.getFixtureSetYearAndMonth (FixtureSetId fsId)

    let allPlayers =
      q.getAllPlayers ()

    let playerNameMap =
      allPlayers
      |> List.map (fun p -> p.Id, p.Name)
      |> Map.ofList

    let getTable (leagueId, window) : LeagueTableDoc option =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))

    let buildLeagueTable
      (leagueName, document, previousTable:LeagueTableDoc option,
        (playerPredictions:Map<PlayerId, (FixtureRecord * PredictionRecord) list>)) =

      let previousTableMap =
        previousTable |> Option.map (fun t -> t.Members |> Map.ofList)

      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
      playerPredictions
      |> Map.map (fun playerId fixturePredictions ->
        fixturePredictions
        |> List.map fixturePredictionToPoints
        |> List.fold (+) PredictionPointsMonoid.Init
        |> fun m ->
          playerNameMap.TryFind playerId
          |> Option.map (fun playerName ->
             { Position = 0
               Movement = 0
               PlayerName = playerName
               Points = m }))
      |> Map.toList
      |> List.filter (snd >> Option.isSome)
      |> List.map (fun (p, o) -> p, o.Value)
      |> standingAlgo
      |> movementAlgo previousTableMap
      |> fun members ->
        { LeagueTableDoc.LeagueName = leagueName
          Members = members }
      |> repo.Insert document

    let membersToPredictionMap f =
      List.map (fun playerId -> playerId, f playerId) >> Map.ofList

    allLeaguesAndMembers deps allPlayers
    |> List.iter (fun (leagueId, leagueName, members) ->
      [ leagueName, LeagueTableDocument (leagueId, Full),
          (getTable (leagueId, WeekInclusive (gwno - 1))),
          members |> membersToPredictionMap q.getPredictionsForPlayer

        leagueName, LeagueTableDocument (leagueId, WeekInclusive gwno),
          None, members |> membersToPredictionMap q.getPredictionsForPlayer

        leagueName, LeagueTableDocument (leagueId, Week gwno),
          None, members |> membersToPredictionMap (q.getPredictionsForPlayerInFixtureSet (FixtureSetId fsId))

        leagueName, LeagueTableDocument (leagueId, Month (year, month)),
          None, members |> membersToPredictionMap (q.getPredictionsForPlayerInMonth (year, month))
      ]
      |> List.iter buildLeagueTable)

  let updateFixtureGraph (deps:Dependencies) _ (_, fId, scoreLine) =
    deps.NonQueries.classifyFixture (fId, scoreLine)

  let updatePlayerFixtureSetsDoc (deps:Dependencies) _ (fsId, _, _) =
    let q =
      deps.Queries
    let gwno =
      q.getFixtureSetGameweekNo fsId
    q.getAllPlayers ()
    |> List.iter (fun player ->
        q.getPredictionsForPlayerInFixtureSet fsId player.Id
        |> List.map fixturePredictionToPoints
        |> List.fold (+) PredictionPointsMonoid.Init
        |> fun m ->
          { GameweekNo = gwno
            AveragePoints = 0.
            PlayerPoints = m }
        |> fun docRow ->
          ElasticSearch.repo deps.ElasticSearch
          |> fun repo ->
            repo.Upsert
              (PlayerFixtureSetsDocument player.Id)
              (PlayerFixtureSetsDoc.Init player.Id)
              (fun pfsd -> { pfsd with FixtureSets = pfsd.FixtureSets.Add (fsId, docRow) }))

  let updateLeagueHistoryWindowDoc (deps:Dependencies) (docF, window, description) =

    let getLeagueTable (leagueId, window) : LeagueTableDoc option =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))

    let leagueIds =
      deps.Queries.getPrivateLeagues ()
      |> List.ofSeq
      |> List.map (fun l -> PrivateLeague l.PrivateLeagueId)
      |> fun l -> GlobalLeague::l

    leagueIds
    |> List.iter (fun leagueId ->
      match getLeagueTable (leagueId, window) with
      | Some table ->
        table.Members
        |> List.tryHead // TODO: everyone in position 1
        |> function
        | Some (_, m) ->
          ElasticSearch.repo deps.ElasticSearch
          |> (fun repo ->
            repo.Upsert (docF leagueId)
              Map.empty
              (fun d ->
                d.Add(window,
                  { LeagueHistoryUnitWinner.PlayerName = m.PlayerName
                    Description = description
                    Points = m.Points })))
        | None -> ()
      | None -> ())

  let updateLeagueHistoryFixtureSetDoc deps _ (FixtureSetId fsId, _, _) =
    deps.Queries.getFixtureSetGameweekNo (FixtureSetId fsId)
    |> fun (GameweekNo gwno) ->
    (LeagueAllFixtureSetHistory, Week gwno, sprintf "Gameweek %i" gwno)
    |> updateLeagueHistoryWindowDoc deps

  let updateLeagueHistoryMonthSetDoc deps _ (FixtureSetId fsId, _, _) =
    deps.Queries.getFixtureSetEarliestKickOff (FixtureSetId fsId)
    |> fun (KickOff ko) ->
    (LeagueAllMonthHistory, Month (ko.DateTime.Year, ko.Month), (ko.ToString("MMMM yyyy")))
    |> updateLeagueHistoryWindowDoc deps

  let updateMatrixDoc deps _ (fsId, fId, resultScoreLine) =

    let q =
      deps.Queries

    let allPlayers =
      q.getAllPlayers ()

    let playerNameMap =
      allPlayers
      |> List.map (fun p -> p.Id, p.Name)
      |> Map.ofList

    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    q.getFixtureSetGameweekNo fsId
    |> fun gwno ->
    FixtureSubscribersAssistance.allLeaguesAndMembers deps allPlayers
    |> List.iter (fun (leagueId, _, members) ->
      (fun (m:MatrixDoc) ->
        { m with
            Columns =
              m.Columns.Add(fId, { m.Columns.[fId] with State = MatrixFixtureState.Classified resultScoreLine })
            Rows =
              members
              |> List.map (fun pId ->
                match m.Rows.TryFind pId with
                | Some mPlayer ->
                  match mPlayer.Predictions.TryFind fId with
                  | Some mPrediction ->
                    Points.getPointsForPrediction resultScoreLine (Some (mPrediction.Prediction, mPrediction.IsDoubleDown))
                    |> fun (m, cat) ->
                      mPlayer.Predictions.Add(fId, { mPrediction with Points = Some (m.Points, cat) })
                      |> fun predictions ->
                        predictions
                        |> Map.toList
                        |> List.sumBy (fun (_, p) ->
                          match p.Points with
                          | Some (points, _) -> points
                          | None -> 0)
                        |> fun totalPoints ->
                          pId, { mPlayer with Predictions = predictions; TotalPoints = totalPoints }
                  | None ->
                    pId, mPlayer
                | None ->
                  pId, { MatrixPlayer.PlayerName = playerNameMap.[pId]; Predictions = Map.empty; TotalPoints = 0 })
              |> Map.ofList
          })
        |> repo.Edit (Matrix (leagueId, gwno))
        |> ignore)

  let updateRealPremTable deps _ _ =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    deps.Queries.getAllFixtures ()
    |> List.ofSeq
    |> List.choose (fun f -> f.ScoreLine |> Option.map (fun sl -> f.TeamLine, sl))
    |> Points.buildTable PremTable.Init
    |> repo.Insert RealPremTable

  let updateFormGuideDoc deps _ (_, fId, _) =
    let repo =
      ElasticSearch.repo deps.ElasticSearch
    let rebuildFormGuide team =
      deps.Queries.getFixturesForTeam team
      |> List.ofSeq
      |> List.sortBy (fun f -> f.KickOff)
      |> List.choose (fun f -> f.ScoreLine |> Option.map (fun s -> f, s))
      |> List.map (fun ({ TeamLine = TeamLine (homeTeam, _); KickOff = ko }, ScoreLine (homeScore, awayScore)) ->
        getScoreResult (ScoreLine (homeScore, awayScore))
        |> fun scoreResult ->
        match if team = homeTeam then H else A with
        | H ->
          { KickOff = ko
            Venue = H
            Result =
              match scoreResult with
              | HomeWin -> W
              | AwayWin -> L
              | Draw -> D
            GoalsFor = homeScore
            GoalsAgainst = awayScore }
        | A ->
          { KickOff = ko
            Venue = A
            Result =
              match scoreResult with
              | HomeWin -> L
              | AwayWin -> W
              | Draw -> D
            GoalsFor = awayScore
            GoalsAgainst = homeScore })
        |> repo.Insert (FormGuideDocument team)

    let { FixtureRecord.TeamLine = TeamLine (homeTeam, awayTeam) } =
      deps.Queries.getFixtureRecord fId

    rebuildFormGuide homeTeam
    rebuildFormGuide awayTeam


  let updateOpenFixtureDetails deps _ (_, _, _) =
    /// needed to update fd with first result when a team appears twice in one gw
    deps.Queries.getAllFixtures ()
    |> List.ofSeq
    |> List.filter (fun f -> f.ScoreLine.IsNone)
    |> List.iter (fun { FixtureRecord.Id = fId; KickOff = ko; TeamLine = TeamLine (home, away) } ->
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
        repo.Insert (FixtureDetailsDocument fId)
          { Id = fId; KickOff = ko; Home = buildColumn deps home; Away = buildColumn deps away })


  let all =
    [ updateFixtureGraph
      updateAllLeagueTables
      updatePlayerFixtureSetsDoc
      updateLeagueHistoryFixtureSetDoc
      updateLeagueHistoryMonthSetDoc
      updateMatrixDoc
      updateRealPremTable
      updateFormGuideDoc
      updateOpenFixtureDetails
    ]

module FixtureAppendedSubscribers =

  open FixtureSubscribersAssistance

  let createFixtureDetails (deps:Dependencies) _ (_, fixture) =
    fixture
    |> (fun { FixtureRecord.Id = fId; KickOff = ko; TeamLine = TeamLine (home, away) } ->
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
        repo.Insert (FixtureDetailsDocument fId)
          { Id = fId; KickOff = ko; Home = buildColumn deps home; Away = buildColumn deps away })

  let createFixture (deps:Dependencies) created (FixtureSetId fsId, fixture) =

    let fixtures =
      deps.Queries.getFixturesInFixtureSet (FixtureSetId fsId)
      |> List.ofSeq

    let (GameweekNo gwno) =
      fixtures
      |> List.head
      |> fun f -> f.GameweekNo

    fixture
    |> (fun { FixtureRecord.Id = FixtureId fId
              KickOff = KickOff ko
              TeamLine = TeamLine (Team home, Team away) } ->
      { FixtureNode.Id = string fId
        FixtureSetId = string fsId
        Created = created
        GameweekNo = gwno
        SortOrder = List.length fixtures
        KickOff = ko
        HomeTeam = home
        AwayTeam = away
        HasKickedOff = false
        HasResult = false
        HomeScore = 0
        AwayScore = 0 })
    |> deps.NonQueries.createFixture (FixtureSetId fsId)

  let updateMatrix (deps:Dependencies) created (FixtureSetId fsId, fixture) =

    let fixtures =
      deps.Queries.getFixturesInFixtureSet (FixtureSetId fsId)
      |> List.ofSeq

    let (GameweekNo gwno) =
      fixtures
      |> List.head
      |> fun f -> f.GameweekNo

    let columns =
      fixtures
      |> List.map (fun f ->
        f.Id,
          { MatrixFixture.TeamLine = f.TeamLine
            KickOff = f.KickOff
            State = MatrixFixtureState.Open
            SortOrder = f.SortOrder
          })
      |> Map.ofList

    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
    FixtureSubscribersAssistance.leagueIdsAndNames deps
    |> List.choose (fun (leagueId, _) -> repo.Read (Matrix (leagueId, (GameweekNo gwno))))
    |> List.map (fun m -> { m with Columns = columns })
    |> List.iter (fun m -> repo.Insert (Matrix (m.LeagueId, (GameweekNo gwno))) m)

  let all =
    [ createFixtureDetails
      createFixture
      updateMatrix
    ]


module FixtureRemovedSubscribers =

  let removeFixture (deps:Dependencies) _ fId =
    deps.NonQueries.deleteFixture fId

  let all =
    [ removeFixture
    ]