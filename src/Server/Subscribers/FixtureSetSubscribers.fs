namespace Server.Subscribers

open FSharp.Core
open Shared
open System
open Server
open Server.Infrastructure

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
      List.ofSeq members)

  let allLeaguesAndMembers deps =
    (GlobalLeague, Global.leagueName, deps.Queries.getAllPlayers()) :: getPrivateLeaguesAndLeagueMembers deps

  let getPlayerPushSubscriptions (deps:Dependencies) : List<PlayerId * PushSubscription> =
    Documents.repo deps.ElasticSearch
    |> fun repo -> repo.Read PlayerPushSubscriptions
    |> Option.defaultValue []
    |> List.distinct

  let getPremTableRow (deps:Dependencies) team =
    let repo = Documents.repo deps.ElasticSearch
    repo.Read RealPremTable
    |> Option.bind (fun (table:PremTable) -> Map.tryFind team table.Rows)
    |> Option.defaultValue PremTableRow.Init

  let getFormGuide (deps:Dependencies) team : FormFixture list =
    let repo = Documents.repo deps.ElasticSearch
    repo.Read (FormGuideDocument team)
    |> Option.defaultValue []
    |> List.sortBy (fun f -> f.KickOff)

  let makeListsOfEqualLength a b =
    [ 1 .. Math.Max (List.length a, List.length b) ]
    |> List.mapi (fun i _ -> List.tryItem i a, List.tryItem i b)

  let buildFormGuide (deps:Dependencies) home away =
    makeListsOfEqualLength (getFormGuide deps home) (getFormGuide deps away)

  let getAttributableYearMonthForFixtureSet =
    List.minBy(fun (f:FixtureRecord) -> f.KickOff)
    >> fun f -> Ko.yearMonth f.KickOff

  let getYearMonthGameweekMap =
    List.groupBy (fun (_, fixtures) -> getAttributableYearMonthForFixtureSet fixtures)
    >> List.map (fun (k, fxs) -> k, fxs |> List.map (fun (gwno:GameweekNo, _) -> gwno))
    >> Map.ofList

module YearMonth =
  let toDateTime (YearMonth (y, m)) = DateTime(y, m, 1)
  let desc = toDateTime >> fun d -> d.ToString("MMMM yyyy")

module FixtureSetCreatedSubscribers =

  open Infrastructure.Push
  open FixtureSubscribersAssistance

  let createFixtureSet (deps:Dependencies) created (FixtureSetId fsId, GameweekNo gwno, fixtures:FixtureRecord list) =
    fixtures
    |> List.minBy (fun { KickOff = ko } -> ko.Raw)
    |> fun { KickOff = minKo } ->
    { FixtureSetNode.Id = string fsId
      GameweekNo = gwno
      Year = minKo.Raw.Year
      Month = minKo.Raw.Month
      Created = created
      IsConcluded = false }
    |> deps.NonQueries.createFixtureSet

    fixtures
    |> List.sortBy (fun { KickOff = ko } -> ko.Raw)
    |> List.mapi (
      FixtureNode.init (FixtureSetId fsId) (GameweekNo gwno) created)
    |> List.iter (deps.NonQueries.createFixture (FixtureSetId fsId))

  let createMatrix (deps:Dependencies) created (fsId, gwno, fixtures:FixtureRecord list) =
    let repo = Documents.repo deps.ElasticSearch
    let columns =
      fixtures
      |> List.map (fun f ->
        f.Id,
        { MatrixFixture.TeamLine = f.TeamLine
          KickOff = f.KickOff
          State = f.State
          SortOrder = f.SortOrder })
      |> Map.ofList
    FixtureSubscribersAssistance.allLeaguesAndMembers deps
    |> List.iter (fun (leagueId, leagueName, players) ->
      let rows =
        players
        |> List.map (fun p -> p.Id, MatrixPlayer.Init p.Name)
        |> Map.ofList
      { FixtureSetId = fsId
        LeagueName = leagueName
        LeagueId = leagueId
        GameweekNo = gwno
        Columns = columns
        Rows = rows }
      |> repo.Insert (Matrix (leagueId, gwno)))

  let notifyPlayers (deps:Dependencies) created (fsId, GameweekNo gwno, _) =
    { PushMessage.Title = sprintf "GW %i fixtures added" gwno
      Body = "Get your predictions in!" }
    |> fun m ->
    FixtureSubscribersAssistance.getPlayerPushSubscriptions deps
    |> List.iter (fun (_, ps) -> deps.PushNotify m ps)

  let createFixtureDetails (deps:Dependencies) _ (_, _, fixtures) =
    fixtures
    |> List.iter (fun { FixtureRecord.Id = fId; TeamLine = TeamLine (home, away) } ->
      let repo = Documents.repo deps.ElasticSearch
      repo.Insert (FixtureDetailsDocument fId)
        { BigUps = []
          Home = getPremTableRow deps home
          Away = getPremTableRow deps away
          FormGuide = buildFormGuide deps home away })

  let all =
    [ createFixtureSet
      createMatrix
      createFixtureDetails
      notifyPlayers
    ]

module FixtureSetConcludedSubscribers =

  let concludeFixtureSet (deps:Dependencies) _ (fsId, _) =
    deps.NonQueries.concludeFixtureSet fsId

  let calculateGlobalGameweekWinner (deps:Dependencies) created (fsId, gwno) =
    Documents.repo deps.ElasticSearch
    |> fun repo ->
    LeagueTableDocument (GlobalLeague, Week gwno)
    |> repo.Read
    |> Option.bind (fun table -> table.Members |> List.tryHead)
    |> Option.map (fun (playerId, m) ->
      let repo = Documents.repo deps.ElasticSearch
      repo.Insert
        GlobalGameweekWinner
        { GlobalGameweekWinner.PlayerId = playerId
          GameweekNo = gwno
          Member = m })
    |> ignore<Unit option>

  let all =
    [ concludeFixtureSet
      calculateGlobalGameweekWinner
    ]

module FixtureKoEditedSubscribers =

  open Infrastructure.Push

  let editFixtureKo (deps:Dependencies) _ (_, fId, ko) =
    deps.NonQueries.editFixtureKo (fId, ko)

  let notifyPlayers (deps:Dependencies) _ (_, fId, ko:KickOff) =
    let { FixtureRecord.TeamLine = TeamLine (Team h, Team a) } = deps.Queries.getFixtureRecord fId
    let m = { PushMessage.Title = "New kickoff time!"; Body = $"""{h} v {a} is now at {ko.Raw.ToString("r")}""" }
    FixtureSubscribersAssistance.getPlayerPushSubscriptions deps
    |> List.iter (fun (_, ps) -> deps.PushNotify m ps)

  let all =
    [ editFixtureKo
      notifyPlayers
    ]

module FixtureKickedOffSubscribers =

  let kickOffFixture (deps:Dependencies) _ (_, fId) =
    deps.NonQueries.kickOffFixture fId

  let updateMatrix (deps:Dependencies) _ (fsId, fId) =

    let q = deps.Queries
    let gwno = q.getFixtureSetGameweekNo fsId
    let fixture = q.getFixtureRecord fId
    let repo = Documents.repo deps.ElasticSearch

    let getPredictionForPlayer pId =
      q.getPlayerPredictionForFixture pId fId
      |> Option.map (fun p ->
        { MatrixPrediction.Prediction = p.ScoreLine
          Modifier = p.Modifier
          Points = None })

    let buildRow (m:MatrixDoc) fId (player:PlayerRecord) =
      let pId = player.Id
      match m.Rows.TryFind pId, getPredictionForPlayer pId with
      | Some pl, Some pr -> pId, { pl with Predictions = pl.Predictions.Add(fId, pr) }
      | Some pl, None -> pId, pl
      | None, Some pr -> pId, { MatrixPlayer.PlayerName = player.Name; Predictions = Map.ofList [ fId, pr ]; TotalPoints = 0 }
      | _ -> pId, MatrixPlayer.Init player.Name

    FixtureSubscribersAssistance.allLeaguesAndMembers deps
    |> List.iter (fun (leagueId, _, members) ->
      repo.Edit (Matrix (leagueId, gwno))
        (fun (m:MatrixDoc) ->
          { m with
              Columns = m.Columns.Add(fId, { m.Columns.[fId] with State = fixture.State })
              Rows = List.map (buildRow m fId) members |> Map.ofList })
      |> ignore<Rresult<unit>>)

  let updatePredictedPremTableForAllPlayers deps _ (_, _) =
    let repo = Documents.repo deps.ElasticSearch
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

  let fixturePredictionToPoints ({ FixtureRecord.State = state }, { PredictionRecord.Modifier = modifier; ScoreLine = pred }) =
    FixtureState.classifiedScoreLine state
    |> Option.map (fun result ->
      let vectors = Points.getPointVectors result pred modifier
      Points.getPointsForPrediction result pred vectors |> fst)
    |> Option.defaultValue PredictionPointsMonoid.Init

  type PositionNumber = PositionNumber of int
  type PositionCollection = PositionCollection of (PlayerId * LeagueTableMember) list

  let sort (ppm:PredictionPointsMonoid) =
    ppm.Points,
    ppm.CorrectScores,
    ppm.CorrectResults

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

  let avergagePointsWithAtLeastOnePrediction =
    List.filter (snd >> List.isEmpty >> not)
    >> List.map (snd >> List.map fixturePredictionToPoints >> List.fold (+) PredictionPointsMonoid.Init)
    >> function | [] -> [ PredictionPointsMonoid.Init ] | m -> m
    >> List.averageBy (fun m -> double m.Points)
    >> int

  let updateAllLeagueTables (deps:Dependencies) _ (fsId, _, _) =
    let q = deps.Queries
    let gwno = q.getFixtureSetGameweekNo fsId
    let yearMonth =
      deps.Queries.getFixturesInFixtureSet fsId
      |> getAttributableYearMonthForFixtureSet
    let allFixtures =
      deps.Queries.getAllFixtureSetsAndFixtures()
      |> List.map (fun (_, gwno, fixtures) -> gwno, fixtures)
    let yearMonthGroups = getYearMonthGameweekMap allFixtures

    let getTable (leagueId, window) : LeagueTableDoc option =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))

    let maximumPoints = function
      | [] -> None
      | _ as members ->
        members
        |> List.maxBy (fun (_, ltm:LeagueTableMember) -> ltm.Points)
        |> fun (playerId:PlayerId, ltm) ->
          Some (playerId, ltm.Points.Points)

    let timeIt (LeagueName leagueName) (doc:Document) desc f g =
      let sw = System.Diagnostics.Stopwatch.StartNew()
      let x = f g
      printfn "%s %A %s : %f" leagueName doc desc sw.Elapsed.TotalSeconds
      x

    let buildLeagueTable
      (leagueName, leagueId, document, previousTable:LeagueTableDoc option, scope,
        (playerPredictions:Map<PlayerRecord, (FixtureRecord * PredictionRecord) list>)) =

      let previousTableMap =
        previousTable |> Option.map (fun t -> t.Members |> Map.ofList)

      let repo = Documents.repo deps.ElasticSearch

      playerPredictions
      |> Map.map (fun p fixturePredictions ->
        fixturePredictions
        |> List.map fixturePredictionToPoints
        |> List.fold (+) PredictionPointsMonoid.Init
        |> fun m -> { LeagueTableMember.Init p.Name with Points = m })
      |> Map.toList
      |> List.map (fun (p, ltm) -> p.Id, ltm)
      |> standingAlgo
      |> movementAlgo previousTableMap
      |> fun members ->
        { LeagueTableDoc.LeagueName = leagueName
          LeagueId = leagueId
          Members = members
          MaximumPoints = maximumPoints members
          LeagueTableScope = scope
          AvergagePointsWithAtLeastOnePrediction = avergagePointsWithAtLeastOnePrediction (Map.toList playerPredictions) }
      |> repo.Insert document

    let membersToPredictionMap f (players:PlayerRecord list) =
      players
      |> List.map (fun p -> p, f p.Id)
      |> Map.ofList

    allLeaguesAndMembers deps
    |> List.iter (fun (leagueId, leagueName, members) ->
      List.iter buildLeagueTable
        [ leagueName, leagueId,
            LeagueTableDocument (leagueId, Full),
            (getTable (leagueId, WeekInclusive (GameweekNo.previous gwno))),
            None,
            membersToPredictionMap q.getPredictionsForPlayer members

          leagueName, leagueId,
            LeagueTableDocument (leagueId, WeekInclusive gwno),
            None,
            Some (IncludesGameweeks (allFixtures |> List.map fst |> List.takeWhile (fun g -> g <= gwno))),
            membersToPredictionMap q.getPredictionsForPlayer members

          leagueName, leagueId,
            LeagueTableDocument (leagueId, Week gwno),
            None,
            Some (OfMonth (YearMonth.toDateTime yearMonth, YearMonth.desc yearMonth)),
            membersToPredictionMap (q.getPredictionsForPlayerInFixtureSet fsId) members

          leagueName, leagueId,
            LeagueTableDocument (leagueId, Month yearMonth),
            None,
            yearMonthGroups.TryFind yearMonth |> Option.map IncludesGameweeks,
            membersToPredictionMap (q.getPredictionsForPlayerInMonth yearMonth) members
        ])

  let updateFixtureGraph (deps:Dependencies) _ (_, fId, scoreLine) =
    deps.NonQueries.classifyFixture (fId, scoreLine)

  let updatePlayerFixtureSetsDoc (deps:Dependencies) _ (fsId, _, _) =
    let q = deps.Queries
    let gwno = q.getFixtureSetGameweekNo fsId
    let repo = Documents.repo deps.ElasticSearch
    q.getAllPlayers ()
    |> List.iter (fun player ->
        let docRow =
          q.getPredictionsForPlayerInFixtureSet fsId player.Id
          |> List.map fixturePredictionToPoints
          |> List.fold (+) PredictionPointsMonoid.Init
          |> fun m ->
            { GameweekNo = gwno
              AveragePoints = 0.
              PlayerPoints = m }
        repo.Upsert
          (PlayerFixtureSetsDocument player.Id)
          (PlayerFixtureSetsDoc.Init player.Id)
          (fun pfsd -> { pfsd with FixtureSets = pfsd.FixtureSets.Add (fsId, docRow) }))

  let (|LeagueTableDocWinner|_|) (table:LeagueTableDoc) =
    List.tryHead table.Members

  let updateLeagueHistoryWindowDoc (deps:Dependencies) (docF, window, description) =

    let getLeagueTable (leagueId, window) : LeagueTableDoc option =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))

    let leagueIds =
      deps.Queries.getPrivateLeagues ()
      |> List.ofSeq
      |> List.map (fun l -> PrivateLeague l.PrivateLeagueId)
      |> fun l -> GlobalLeague::l

    leagueIds
    |> List.iter (fun leagueId ->
      match getLeagueTable (leagueId, window) with
      // TODO: everyone in position 1
      // TODO: add league table doc scope
      | Some (LeagueTableDocWinner (_, m)) ->
          let repo = Documents.repo deps.ElasticSearch
          repo.Upsert (docF leagueId)
            Map.empty
            (fun d ->
              d.Add(window,
                { LeagueHistoryUnitWinner.PlayerName = m.PlayerName
                  Description = description
                  Points = m.Points }))
      | _ -> ())

  let updateLeagueHistoryFixtureSetDoc deps _ (fsId, _, _) =
    let (GameweekNo gwnoi as gwno) = deps.Queries.getFixtureSetGameweekNo fsId
    (LeagueAllFixtureSetHistory, Week gwno, sprintf "Gameweek %i" gwnoi)
    |> updateLeagueHistoryWindowDoc deps

  let updateLeagueHistoryMonthSetDoc deps _ (fsId, _, _) =
    let ym = deps.Queries.getFixturesInFixtureSet fsId |> getAttributableYearMonthForFixtureSet
    (LeagueAllMonthHistory, Month ym, (YearMonth.toDateTime ym).ToString("MMMM yyyy"))
    |> updateLeagueHistoryWindowDoc deps

  let updateMatrixDoc deps _ (fsId, fId, resultScoreLine) =

    let q = deps.Queries
    let gwno = q.getFixtureSetGameweekNo fsId
    let fixture = q.getFixtureRecord fId
    let repo = Documents.repo deps.ElasticSearch

    let getPredictionForPlayer pId =
      q.getPlayerPredictionForFixture pId fId
      |> Option.map (fun p ->
        Points.getPointVectors resultScoreLine p.ScoreLine p.Modifier
        |> Points.getPointsForPrediction resultScoreLine p.ScoreLine
        |> fun (ppm, cat) ->
        { MatrixPrediction.Prediction = p.ScoreLine
          Modifier = p.Modifier
          Points = Some (ppm.Points, cat) })

    let buildRow (m:MatrixDoc) fId (player:PlayerRecord) =
      let pId = player.Id
      match m.Rows.TryFind pId, getPredictionForPlayer pId with
      | Some mPlayer, Some mPrediction ->
          let predictions = mPlayer.Predictions.Add(fId, mPrediction)
          let totalPoints =
            Map.toList predictions
            |> List.sumBy (fun (_, p) -> match p.Points with | Some (points, _) -> points | None -> 0)
          pId, { mPlayer with Predictions = predictions; TotalPoints = totalPoints }
      | Some mPlayer, None -> pId, mPlayer
      | None, Some mPrediction -> pId, { MatrixPlayer.Init player.Name with Predictions = Map.ofList [ fId, mPrediction ] }
      | _ -> pId, MatrixPlayer.Init player.Name

    FixtureSubscribersAssistance.allLeaguesAndMembers deps
    |> List.iter (fun (leagueId, _, members) ->
      repo.Edit (Matrix (leagueId, gwno))
        (fun (m:MatrixDoc) ->
          { m with
              Columns = m.Columns.Add(fId, { m.Columns.[fId] with State = fixture.State })
              Rows = List.map (buildRow m fId) members |> Map.ofList })
      |> ignore<Rresult<unit>>)

  let updateRealPremTable deps _ _ =
    let repo = Documents.repo deps.ElasticSearch
    deps.Queries.getAllFixtures ()
    |> List.ofSeq
    |> List.choose (fun f -> FixtureState.classifiedScoreLine f.State |> Option.map (fun sl -> f.TeamLine, sl))
    |> Points.buildTable PremTable.Init
    |> repo.Insert RealPremTable

  let updateFormGuideDoc deps _ (_, fId, _) =
    let repo = Documents.repo deps.ElasticSearch

    let buildFormFixture team ({ TeamLine = (TeamLine(h, a) as tl) } as p:FixtureRecord, scoreline) =
      { FormFixture.KickOff = p.KickOff
        Scoreline = scoreline
        TeamLine = tl
        GameweekNo = p.GameweekNo
        Venue = if h = team then FormVenue.H else FormVenue.A
        Opponent = if h = team then a else h
        Result =
          match getScoreResult scoreline with
          | HomeWin when team = h -> FormResult.W
          | AwayWin when team = a -> FormResult.W
          | Draw -> FormResult.D
          | _ -> FormResult.L }

    let rebuildFormGuide team =
      deps.Queries.getFixturesForTeam team
      |> List.ofSeq
      |> List.sortByDescending (fun f -> f.KickOff)
      |> List.choose (fun f -> FixtureState.classifiedScoreLine f.State |> Option.map (fun sl -> f, sl))
      |> List.truncate 5
      |> List.map (buildFormFixture team)
      |> repo.Insert (FormGuideDocument team)

    let { FixtureRecord.TeamLine = TeamLine (homeTeam, awayTeam) } = deps.Queries.getFixtureRecord fId
    rebuildFormGuide homeTeam
    rebuildFormGuide awayTeam


  let updateOpenFixtureDetails deps _ (_, _, _) =
    /// needed to update fd with first result when a team appears twice in one gw
    deps.Queries.getOpenFixtures ()
    |> List.ofSeq
    |> List.iter (fun { FixtureRecord.Id = fId; TeamLine = TeamLine (home, away) } ->
      Documents.repo deps.ElasticSearch
      |> fun repo ->
        repo.Upsert
          (FixtureDetailsDocument fId)
          FixtureDetails.Init
          (fun fd ->
            { fd with
                  Home = getPremTableRow deps home
                  Away = getPremTableRow deps away
                  FormGuide = buildFormGuide deps home away }))


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
    |> (fun { FixtureRecord.Id = fId; TeamLine = TeamLine (home, away) } ->
      let repo = Documents.repo deps.ElasticSearch
      repo.Insert (FixtureDetailsDocument fId)
        { BigUps = []
          Home = getPremTableRow deps home
          Away = getPremTableRow deps away
          FormGuide = buildFormGuide deps home away })


  let createFixture (deps:Dependencies) created (fsId, fixture) =
    let gwno = deps.Queries.getFixtureSetGameweekNo fsId
    FixtureNode.init fsId gwno created 0 fixture
    |> deps.NonQueries.createFixture fsId
    deps.Queries.getFixturesInFixtureSet fsId
    |> List.sortBy(fun f -> f.KickOff.Raw)
    |> List.mapi(fun i f -> f.Id, i)
    |> List.iter(deps.NonQueries.editFixtureSortOrder)


  let updateMatrix (deps:Dependencies) created (fsId, fixture) =
    let fixtures = deps.Queries.getFixturesInFixtureSet fsId
    let gwno = deps.Queries.getFixtureSetGameweekNo fsId

    let columns =
      fixtures
      |> List.sortBy(fun f -> f.SortOrder)
      |> List.map (fun f ->
        f.Id,
          { MatrixFixture.TeamLine = f.TeamLine
            KickOff = f.KickOff
            State = f.State
            SortOrder = f.SortOrder
          })
      |> Map.ofList

    Documents.repo deps.ElasticSearch
    |> fun repo ->
    FixtureSubscribersAssistance.leagueIdsAndNames deps
    |> List.choose (fun (leagueId, _) -> repo.Read (Matrix (leagueId, gwno)))
    |> List.map (fun m -> { m with Columns = columns })
    |> List.iter (fun m -> repo.Insert (Matrix (m.LeagueId, gwno)) m)

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
