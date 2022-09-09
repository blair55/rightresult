namespace Server.Application

open System
open FSharp.Data
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Server.Commands
open Server.Queries
open Shared
open Server.Infrastructure
open Server.Elevated
open System.IO
open Server
open Config
open Microsoft.AspNetCore

module Neighbours =

  let build items =
    let paired = List.pairwise items
    let find a b x = List.tryFind (a >> (=) x) paired |> Option.map b
    List.fold (fun m x -> Map.add x (find snd fst x, find fst snd x) m) Map.empty items

  let get m x =
    Map.tryFind x m |> Option.bind fst,
    Map.tryFind x m |> Option.bind snd

module Protocol =

  let buildProtocol (handleCommand:Command -> Ars<Unit>) (deps:Dependencies) : HttpHandler =

    let q = deps.Queries
    let now = deps.Now
    let validateToken = deps.ValidateToken
    let config = deps.ApplicationConfiguration

    let getPoints { FixtureRecord.State = state } pred =
      match state, pred with
      | FixtureState.Classified result, Some { PredictionRecord.Modifier = modifier; ScoreLine = pred } ->
        let vectors = Points.getPointVectors result pred modifier
        let {PredictionPointsMonoid.Points = p} = Points.getPointsForPrediction result pred vectors
        p, vectors
      | _ -> 0, []

    let isAnyDoubleDownFixtureForGameweekAlreadyKickedOff gwno =
      List.exists (fun ({ FixtureRecord.GameweekNo = fgwno; KickOff = ko }, pred:PredictionRecord option) ->
        match pred with
        | Some p when PredictionModifier.isDoubleDown p.Modifier && fgwno = gwno && (now()) > ko.Raw -> true
        | _ -> false)

    let isAnyFixtureForGameweekClosed =
      List.exists (fun ({ FixtureRecord.State = state; KickOff = ko }, _) ->
        match state with
          | FixtureState.InPlay _
          | FixtureState.Classified _
          | FixtureState.Open _ when (now()) > ko.Raw -> true
          | _ -> false)

    let fixtureStateFirstMinuteHack now (f:FixtureRecord) =
      match f.State with
      | FixtureState.Open ko when (now()) > ko.Raw -> FixtureState.InPlay(ScoreLine.Init, MinutesPlayed.init)
      | s -> s

    let ordinal (num:int) =
      match num % 100 with
      | 11
      | 12
      | 13 -> "th"
      | _ ->
          match num % 10 with
          | 1 -> "st"
          | 2 -> "nd"
          | 3 -> "rd"
          | _ -> "th"

    let gwTable playerId gwno =
      let repo = Documents.repo deps.ElasticSearch
      let table = repo.Read (LeagueTableDocument (GlobalLeague, Week gwno))
      let pointsAndPosition =
        table
        |> Option.bind (fun (t:LeagueTableDoc) -> Map.ofList t.Members |> Map.tryFind playerId)
        |> Option.map (fun t -> t.Points.Points, t.Position, ordinal t.Position)
      table
      |> Option.map (fun t ->
        { GameweekNo = gwno
          PlayerId = playerId
          AveragePoints = t.AvergagePointsWithAtLeastOnePrediction
          MaximumPoints = t.MaximumPoints
          Player = pointsAndPosition })

    let getGameweekFixtures (GameweekNo gw as gwno) (jwtPlayer:Jwt.JwtPlayer) : Rresult<GameweekFixturesViewModel> =
      let fixturesAndPredictions =
        q.getPlayerFixtureSet (PlayerId jwtPlayer.playerId) gwno
        |> List.ofSeq
        |> List.sortBy (fun (f, _) -> f.SortOrder)
      match q.getGameweekNoFixtureSet gwno with
      | None -> ValidationError $"could not find gwno {gw}" |> Error
      | Some fsId ->
        let gwNeighbours = Neighbours.build (q.getGameweekNos() |> List.sort)
        let fxNeighbours = Neighbours.build (fixturesAndPredictions |> List.map (fun (f, _) -> f.Id))
        let bigUpState { FixtureRecord.KickOff = ko }  (pred:PredictionRecord option) =
          let hasBigUpOnAnyFixture =
            fixturesAndPredictions
            |> List.exists (function _, Some { Modifier = PredictionModifier.BigUp } -> true | _ -> false)
          let isLessThanOneHourBeforeKickOff = Ko.isLessThanOneHourBeforeKickOff (now()) ko
          match hasBigUpOnAnyFixture, isLessThanOneHourBeforeKickOff, pred with
          | _, _, Some { Modifier = PredictionModifier.BigUp } -> BigUpState.Set
          | true, _, _
          | _, true, _ -> BigUpState.Unavailable
          | _ -> BigUpState.Available
        fixturesAndPredictions
        |> List.map (fun (f, pred) ->
          let repo = Documents.repo deps.ElasticSearch
          let fixtureDetails = repo.Read (FixtureDetailsDocument f.Id)
          let grid =
            pred
            |> Option.map (fun p -> p.ScoreLine)
            |> PredictionGrid.init (PredictionGrid.Dims (5, 3))
          f.Id,
          { FixturePredictionViewModel.Id = f.Id
            FixtureSetId = f.FixtureSetId
            GameweekNo = f.GameweekNo
            SortOrder = f.SortOrder
            TeamLine = f.TeamLine
            KickOff = KickoffComponents.build f.KickOff
            FixtureDetails = fixtureDetails
            State = fixtureStateFirstMinuteHack now f
            Prediction = pred |> Option.map (fun p -> p.ScoreLine, p.Modifier)
            PredictionGrid = grid
            Points = getPoints f pred
            BigUpState = bigUpState f pred
            InProgress = false
            Neighbours = Neighbours.get fxNeighbours f.Id })
        |> Map.ofList
        |> fun fixtures ->
          Ok { GameweekFixturesViewModel.GameweekNo = gwno
               FixtureSetId = fsId
               Fixtures = fixtures
               IsDoubleDownAvailable = not <| isAnyDoubleDownFixtureForGameweekAlreadyKickedOff gwno fixturesAndPredictions
               Neighbours = Neighbours.get gwNeighbours gwno
               GlobalGameweekStats =
                 if isAnyFixtureForGameweekClosed fixturesAndPredictions
                 then gwTable (PlayerId jwtPlayer.playerId) gwno else None
             }

    let getPlayerGameweek playerId gwno : Rresult<PlayerGameweekViewModel> =
      let fixturesAndPredictions =
        q.getPlayerFixtureSet playerId gwno |> List.ofSeq
      match q.getPlayer playerId, fixturesAndPredictions with
      | None, _ -> ValidationError "could not get player" |> Error
      | _, [] -> ValidationError "could not get playerfixture set" |> Error
      | Some player, _::_ ->
        let gwNeighbours = Neighbours.build (q.getGameweekNos() |> List.sort)
        { PlayerId = player.Id
          PlayerName = player.Name
          GameweekNo = gwno
          Neighbours = Neighbours.get gwNeighbours gwno
          GlobalGameweekStats =
            if isAnyFixtureForGameweekClosed fixturesAndPredictions
            then gwTable player.Id gwno else None
          Fixtures =
            fixturesAndPredictions
            |> List.map (fun (f, pred) ->
              f.Id,
              { PlayerGameweekViewModelRow.FixtureId = f.Id
                FixtureSetId = f.FixtureSetId
                IsExpanded = false
                TeamLine = f.TeamLine
                KickOff = KickoffComponents.build f.KickOff
                SortOrder = f.SortOrder
                Prediction =
                  pred
                  |> Option.bind (fun p ->
                    match p.Modifier, Ko.hasKickedOff (now()) f.KickOff with
                    | PredictionModifier.BigUp, _
                    | _, true -> Some (p.ScoreLine, p.Modifier)
                    | _ -> None)
                Points = getPoints f pred
                State = fixtureStateFirstMinuteHack now f })
             |> Map.ofList
        }
        |> Ok

    let getLeaguesPlayerIsIn (jwtPlayer:Jwt.JwtPlayer) : Map<PrivateLeagueId, PlayerLeagueViewModel> =
      PlayerId jwtPlayer.playerId
      |> q.getLeaguesPlayerIsIn
      |> Seq.map(fun league ->
        league.PrivateLeagueId,
        { PlayerLeagueViewModel.Position = 0
          Movement = 0
          LeagueName = league.LeagueName
        })
      |> Map.ofSeq

    let createLeague (appToken:AppToken) leagueName : Ars<PrivateLeagueId> =
      let newLeagueId =
        PrivateLeagueId (Guid.NewGuid())
      let createCreateLeagueCommand leagueId (jwtPlayer:Jwt.JwtPlayer) =
        PrivateLeagueCommand(leagueId, CreateLeague(PlayerId jwtPlayer.playerId, leagueName))
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map (createCreateLeagueCommand newLeagueId)
        // >> AsyncResult.mapError CommandApplicationError
        >> AsyncResult.bind handleCommand
        >> AsyncResult.map (fun () -> newLeagueId))

    let joinLeague (appToken:AppToken) leagueId : Ars<PrivateLeagueId> =
      let joinLeagueCommand (jwtPlayer:Jwt.JwtPlayer) =
        PrivateLeagueCommand(leagueId, JoinLeague(PlayerId jwtPlayer.playerId))
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map joinLeagueCommand
        >> AsyncResult.bind handleCommand
        >> AsyncResult.map (fun () -> leagueId))

    let leaveLeague (appToken:AppToken) leagueId : Ars<Unit> =
      let leaveLeagueCommand (jwtPlayer:Jwt.JwtPlayer) =
        PrivateLeagueCommand(leagueId, LeaveLeague(PlayerId jwtPlayer.playerId))
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map leaveLeagueCommand
        >> AsyncResult.bind handleCommand)

    let makePrediction (appToken:AppToken) pred = // (PredictionAction (fsId, fId, team)) =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        match pred with
        | PredictionAction.SetScoreline (fsId, fId, sl) -> fsId, fId, SetScoreLine sl
        | PredictionAction.IncrementScore (fsId, fId, Home) -> fsId, fId, SimplePredictionCommand IncHomeScore
        | PredictionAction.DecrementScore (fsId, fId, Home) -> fsId, fId, SimplePredictionCommand DecHomeScore
        | PredictionAction.IncrementScore (fsId, fId, Away) -> fsId, fId, SimplePredictionCommand IncAwayScore
        | PredictionAction.DecrementScore (fsId, fId, Away) -> fsId, fId, SimplePredictionCommand DecAwayScore
        |> fun (fsId, fId, c) ->
          DatedPredictionCommand (c, fId, PredictionEditDate (now()))
          |> fun cmd -> PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, cmd)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun () -> AsyncResult.retn pred))

    let doDoubleDown (appToken:AppToken) (fsId, fId) =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        DatedPredictionCommand (DoubleDown, fId, PredictionEditDate (now()))
        |> fun cmd -> PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, cmd)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn (fsId, fId)))

    let doBigUp (appToken:AppToken) (fsId, fId) =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        DatedPredictionCommand (BigUp, fId, PredictionEditDate (now()))
        |> fun cmd -> PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, cmd)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn (fsId, fId)))

    let removeDownDown (appToken:AppToken) fsId =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, PredictionEditDate (now()) |> RemoveDoubleDown)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn fsId))

    let subscribeToPush (appToken:AppToken) subscription : Ars<Unit> =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        PlayerCommand (PlayerId jwtPlayer.playerId, SubscribeToPush subscription)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand)

    let submitFeedback (config:ApplicationConfiguration) feedback (jwtPlayer:Jwt.JwtPlayer) =
      let now = (now()).ToString("s")
      sprintf "%s\nPLAYER: %s (%s)\n%s\n----------------\n\n" now jwtPlayer.name jwtPlayer.playerId feedback
      |> fun s -> File.AppendAllText(config.feedbackFilePath, s)

    let resultOfOption e = function
      | Some x -> Ok x
      | None -> Error e

    let getPlayerPointsTotal playerId : Rresult<PredictionPointsMonoid> =
      deps.ElasticSearch
      |> (Documents.repo
      >> fun repo -> repo.Read (LeagueTableDocument (GlobalLeague, Full))
      >> resultOfOption (ValidationError "League not found")
      >> Result.bind (fun (league:LeagueTableDoc) ->
        league.Members
        |> List.tryFind (fun (pId, _) -> pId = playerId)
        |> resultOfOption (ValidationError "Player not found"))
      >> Result.map (fun (_, m) -> m.Points))

    let getPlayerFixtureSets playerId =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (PlayerFixtureSetsDocument playerId)
      |> resultOfOption (ValidationError "Player not found")

    let getPrivateLeagueInfo leagueId =
      q.getPrivateLeague leagueId
      |> function
      | Some l -> Ok { LeagueName = l.LeagueName }
      | None -> ValidationError ("could not find league") |> Error

    let getLeagueWindow leagueId window : Rresult<LeagueTableDoc> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))
      |> resultOfOption (ValidationError "League not found")
      |> Result.map (fun t -> { t with Members = List.sortBy (fun (_, p) -> p.Position) t.Members })

    let getAllPlayers _ : PlayerViewModel list =
      q.getAllPlayers ()
      |> Seq.map (fun p -> { PlayerViewModel.Id = p.Id; Name = p.Name })
      |> List.ofSeq
      // |> List.sortWith (fun { Name = PlayerName a } { Name = PlayerName b } -> a.CompareTo b)
      |> List.sortBy (fun { Name = PlayerName a } -> a.ToLower())

    let getPlayerViewModel playerId =
      q.getPlayer playerId
      |> resultOfOption (ValidationError "Player not found")
      |> Result.bind (fun p -> Ok { PlayerViewModel.Id = p.Id; Name = p.Name })

    let getLeagueHistoryFixtureSets leagueId : Rresult<LeagueHistoryDoc> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueAllFixtureSetHistory leagueId)
      |> resultOfOption (ValidationError "League fixture set history not found")

    let getLeagueHistoryMonths leagueId : Rresult<LeagueHistoryDoc> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueAllMonthHistory leagueId)
      |> resultOfOption (ValidationError "League month history not found")

    let getLeagueMatrixForGameweek leagueId gwno : Rresult<MatrixDoc> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (Matrix (leagueId, gwno))
      |> resultOfOption (ValidationError "League matrix not found")

    let getGlobalGameweekWinner () : Rresult<GlobalGameweekWinner option> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read GlobalGameweekWinner
      |> Ok

    let getRealPremTable () : PremTable =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read RealPremTable
      |> Option.defaultValue PremTable.Init

    let getPredictedPremTable (jwtPlayer:Jwt.JwtPlayer) : PremTable =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (PredictedPremTable <| PlayerId jwtPlayer.playerId)
      |> Option.defaultValue PremTable.Init

    let getFixtureDetails fId : Rresult<FixtureDetails> =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Read (FixtureDetailsDocument fId)
      |> resultOfOption (ValidationError "Fixture details not found")

    let getHomePageBigUps () =
      q.getHomePageBigUps ()
      |> List.map (fun (f, pred, player) ->
        { BigUpViewModel.TeamLine = f.TeamLine
          ScoreLine = pred.ScoreLine
          PlayerName = player.Name
          PlayerId = player.Id })

    let vt =
      validateToken

    let protocol =
      { getFixturesLength = vt >> Result.map (fun _ -> q.getFixturesLength()) >> Async.retn
        getMaxGameweekNo = vt >> Result.map (fun _ -> q.getMaxGameweekNo() |> Option.defaultValue (GameweekNo 1)) >> Async.retn
        getPlayerLeagues = vt >> Result.map getLeaguesPlayerIsIn >> Async.retn
        getPrivateLeagueInfo = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getPrivateLeagueInfo leagueId) >> Async.retn)
        getLeagueTable = fun leagueId window t -> t |> (vt >> Result.bind (fun _ -> getLeagueWindow leagueId window) >> Async.retn)
        getAllPlayers = vt >> Result.map getAllPlayers >> Async.retn
        getMyProfile = vt >> Result.bind (fun p -> getPlayerViewModel (PlayerId p.playerId)) >> Async.retn
        getMyPointsTotal = vt >> Result.bind (fun p -> getPlayerPointsTotal (PlayerId p.playerId)) >> Async.retn
        getPlayerInfo = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerViewModel playerId) >> Async.retn)
        getPlayerPointsTotal = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerPointsTotal playerId) >> Async.retn)
        getPlayerFixtureSets = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerFixtureSets playerId) >> Async.retn)
        getPlayerGameweek = fun playerId gwno t -> t |> (vt >> Result.bind (fun _ -> getPlayerGameweek playerId gwno) >> Async.retn)
        getNewFixtureSet = vt >> (fun _ -> FixtureSourcing.getNewFixtureSetViewModel deps) >> AsyncResult.retn
        getLeagueHistoryFixtureSets = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getLeagueHistoryFixtureSets leagueId) >> Async.retn)
        getLeagueHistoryMonths = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getLeagueHistoryMonths leagueId) >> Async.retn)
        getDateFormat = fun dateTime format t -> t |> (vt >> Result.map (fun _ -> dateTime.ToString(format)) >> Async.retn)
        getLeagueMatrix = fun leagueId gwno t -> t |> (vt >> Result.bind (fun _ -> getLeagueMatrixForGameweek leagueId gwno) >> Async.retn)
        getGlobalGameweekWinner = vt >> fun _ -> getGlobalGameweekWinner () |> Async.retn
        getRealPremTable = vt >> Result.map (fun _ -> getRealPremTable ()) >> Async.retn
        getPredictedPremTable = vt >> Result.map getPredictedPremTable >> Async.retn
        getFixtureDetails = fun t fId -> t |> (vt >> fun _ -> getFixtureDetails fId |> Async.retn)
        getEarliestOpenGwno = vt >> Result.map (fun _ -> q.getEarliestOpenGwno() |> Option.orElse (q.getMaxGameweekNo()) |> Option.defaultValue (GameweekNo 1)) >> Async.retn
        getGameweekFixtures = fun t gwno -> t |> (vt >> Result.bind (getGameweekFixtures gwno) >> Async.retn)
        getHomePageBigUps = vt >> Result.map (fun _ -> getHomePageBigUps ()) >> Async.retn

        submitFeedback = fun fb t -> t |> (vt >> Result.map (submitFeedback config fb) >> Async.retn)
        addNewFixtureSet = vt >> (fun _ -> FixtureSourcing.addNewFixtureSet deps) >> handleCommand
        prediction = makePrediction
        doubleDown = doDoubleDown
        bigUp = doBigUp
        removeDoubleDown = removeDownDown
        createLeague = createLeague
        joinLeague = joinLeague
        leaveLeague = leaveLeague
        subscribeToPush = subscribeToPush
      }

    let errorHandler (ex: Exception) (routeInfo: RouteInfo<Http.HttpContext>) =
      printfn "Error at %s on method %s" routeInfo.path routeInfo.methodName
      Propagate ex

    Remoting.createApi()
    |> Remoting.withRouteBuilder Routes.builder
    |> Remoting.withErrorHandler errorHandler
    |> Remoting.fromValue protocol
    // |> Remoting.fromContext protocol
    |> Remoting.buildHttpHandler

