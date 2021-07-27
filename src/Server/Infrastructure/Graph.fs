module Server.Infrastructure.Graph

open System
open Shared
open Server.Elevated
open Server.Queries
open Neo4jClient
open Neo4jClient.Cypher

let client url =
  let c = new GraphClient(Uri(url))//AuthTokens.Basic("username", "pasSW0rd"))
  c.ConnectAsync().Wait()
  c

let synchronously x = Async.AwaitTask x |> Async.RunSynchronously

let deleteAll (graph:IGraphClient) =
  graph.Cypher
    .Match("(n)")
    .DetachDelete("n")
    .ExecuteWithoutResultsAsync()
    .Wait()

let private buildFixtureSetId (s:string) =
  FixtureSetId (Guid.Parse s)

let private buildFixtureRecord (f:FixtureNode) =
  { Id = FixtureId (Guid.Parse f.Id)
    FixtureSetId = buildFixtureSetId f.FixtureSetId
    GameweekNo = GameweekNo f.GameweekNo
    KickOff = KickOff f.KickOff
    TeamLine = TeamLine (Team f.HomeTeam, Team f.AwayTeam)
    ScoreLine = if f.HasResult then ScoreLine (Score f.HomeScore, Score f.AwayScore) |> Some else None
    SortOrder = f.SortOrder
    HasKickedOff = f.HasKickedOff
  }

let private buildPredictionRecord (p:PredictionNode) =
  { PlayerId = PlayerId p.PlayerId
    FixtureId = FixtureId (Guid.Parse p.FixtureId)
    IsDoubleDown = p.IsDoubleDown
    ScoreLine = ScoreLine (Score p.HomeScore, Score p.AwayScore)
    Created = p.Created
  }

let private buildPlayerRecord (p:PlayerNode) =
  { PlayerRecord.Id = PlayerId p.Id
    Name = PlayerName p.Name
  }

let private buildLeagueRecord (l:LeagueNode) =
  { LeagueRecord.LeagueName = LeagueName l.Name
    PrivateLeagueId = PrivateLeagueId (Guid.Parse l.Id)
  }

// TODO inject url only (not gc)
let queries (gc:GraphClient) : Queries =
  { getKickedOffFixtures = fun now ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> now > f.KickOff)
        .AndWhere(fun (f:FixtureNode) -> f.HasKickedOff = false)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord

    getAllFixtures = fun () ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord

    getFixturesInFixtureSet = fun (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord

    getFixturesInLatestFixtureSet = fun () ->
      gc.Cypher
        .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .With("f, fs, fs.Id as fsId")
        .Return(fun f fsId -> f.CollectAs<FixtureNode>(), fsId.As<Guid>(), Return.As<int>("max(fs.GameweekNo)"))
        .Limit(Nullable<int>(1))
        .ResultsAsync.Result
      |> Seq.tryHead
      |> Option.map (fun (fixtures, fixtureSetId, maxGwno) ->
          FixtureSetId fixtureSetId, GameweekNo maxGwno, fixtures |> Seq.map buildFixtureRecord)

    getFixturesAwaitingResults = fun () ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.HasKickedOff = true)
        .AndWhere(fun (f:FixtureNode) -> f.HasResult = false)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord

    getPlayerPredictionsByFixture = fun (PlayerId playerId) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .OptionalMatch("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f)")
        .Where(fun (player:PlayerNode) -> player.Id = playerId)
        .Return(fun f pred -> f.As<FixtureNode>(), pred.As<PredictionNode>())
        .ResultsAsync.Result
      |> Seq.map (fun (f, p) ->
        buildFixtureRecord f, if box p |> isNull then None else buildPredictionRecord p |> Some)

    getPlayerPredictionForFixture = fun (PlayerId playerId) (FixtureId fId) ->
      gc.Cypher
        .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f:Fixture)")
        .Where(fun (player:PlayerNode) -> player.Id = playerId)
        .AndWhere(fun (f:FixtureNode) -> f.Id = string fId)
        .Return<PredictionNode>("pred")
        .ResultsAsync.Result
      |> Seq.map buildPredictionRecord
      |> Seq.tryHead

    getFixtureSetEarliestKickOff = fun (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Return(fun () -> Return.As<DateTime>("min(f.KickOff)"))
        .ResultsAsync.Result
      |> Seq.head
      |> KickOff

    getPredictionsForPlayer = fun (PlayerId playerId) ->
      gc.Cypher
        .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
        .Where(fun (player:PlayerNode) -> player.Id = playerId)
        .Return(fun pred fixture -> fixture.As<FixtureNode>(), pred.As<PredictionNode>())
        .ResultsAsync.Result
      |> List.ofSeq
      |> List.map (fun (fixtureNode, predictionNode) ->
        buildFixtureRecord fixtureNode, buildPredictionRecord predictionNode)

    getPredictionsForPlayerInFixtureSet = fun (FixtureSetId fsId) (PlayerId playerId) ->
      gc.Cypher
        .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .AndWhere(fun (player:PlayerNode) -> player.Id = playerId)
        .Return(fun fixture pred -> fixture.As<FixtureNode>(), pred.As<PredictionNode>())
        .ResultsAsync.Result
      |> List.ofSeq
      |> List.map (fun (fixtureNode, predictionNode) ->
        buildFixtureRecord fixtureNode, buildPredictionRecord predictionNode)

    getPredictionsForPlayerInMonth = fun (year, month) (PlayerId playerId) ->
      gc.Cypher
        .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Year = year)
        .AndWhere(fun (fs:FixtureSetNode) -> fs.Month = month)
        .AndWhere(fun (player:PlayerNode) -> player.Id = playerId)
        .Return(fun fixture pred -> fixture.As<FixtureNode>(), pred.As<PredictionNode>())
        .ResultsAsync.Result
      |> List.ofSeq
      |> List.map (fun (fixtureNode, predictionNode) ->
        buildFixtureRecord fixtureNode, buildPredictionRecord predictionNode)

    getPlayersInPrivateLeague = fun (PrivateLeagueId privateLeagueId) ->
      gc.Cypher
        .Match("(p:Player)-[r:IN_LEAGUE]->(l:League)")
        .Where(fun (l:LeagueNode) -> l.Id = string privateLeagueId)
        .Return<PlayerNode>("p")
        .ResultsAsync.Result
      |> List.ofSeq
      |> List.map buildPlayerRecord

    getAllPlayers = fun () ->
      gc.Cypher
        .Match("(p:Player)")
        .Return<PlayerNode>("p")
        .ResultsAsync.Result
      |> Seq.map buildPlayerRecord
      |> List.ofSeq

    getFixtureSetGameweekNo = fun (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Return<FixtureSetNode>("fs")
        .ResultsAsync.Result
      |> Seq.head
      |> fun fs -> GameweekNo fs.GameweekNo

    getFixtureSetYearAndMonth = fun (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Return<FixtureSetNode>("fs")
        .ResultsAsync.Result
      |> Seq.head
      |> fun fs -> fs.Year, fs.Month

    getFixtureRecord = fun (FixtureId fId) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.head
      |> buildFixtureRecord

    getUnconcludedFixtureSets = fun () ->
      gc.Cypher
        .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.IsConcluded = false)
        .Return(fun fs f -> fs.As<FixtureSetNode>(), f.CollectAs<FixtureNode>())
        .ResultsAsync.Result
      |> Seq.map (fun (fs, fixtures) ->
          fs.Id |> buildFixtureSetId,
          fs.GameweekNo |> GameweekNo,
          fixtures |> List.ofSeq |> List.map buildFixtureRecord)

    getPrivateLeagues = fun () ->
      gc.Cypher
        .Match("(l:League)")
        .Return<LeagueNode>("l")
        .ResultsAsync.Result
      |> Seq.map buildLeagueRecord

    getPrivateLeaguesAndMembers = fun () ->
      gc.Cypher
        .Match("(player:Player)-[:IN_LEAGUE]->(league:League)")
        .Return(fun player league -> league.As<LeagueNode>(), player.CollectAs<PlayerNode>())
        .ResultsAsync.Result
      |> Seq.map (fun (league, players) ->
        buildLeagueRecord league, players |> Seq.map buildPlayerRecord)

    getFixturesLength = fun () ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Return(fun () -> Return.As<int>("count(f)"))
        .ResultsAsync.Result
      |> Seq.exactlyOne

    getLeaguesPlayerIsIn = fun (PlayerId playerId) ->
      gc.Cypher
        .Match("(player)-[:IN_LEAGUE]->(league:League)")
        .Where(fun (player:PlayerNode) -> player.Id = playerId)
        .Return<LeagueNode>("league")
        .ResultsAsync.Result
      |> Seq.map buildLeagueRecord

    getPlayerFixtureSet = fun (PlayerId playerId) (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet {Id:$fsId})")
        .OptionalMatch("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f)-[:IN_FIXTURESET]->(fs)")
        .WithParam("fsId", fsId)
        .Where(fun (player:PlayerNode) -> player.Id = playerId)
        .Return(fun f pred -> f.As<FixtureNode>(), pred.As<PredictionNode>())
        .ResultsAsync.Result
      |> Seq.map(fun (f, p) ->
        buildFixtureRecord f, if box p |> isNull then None else buildPredictionRecord p |> Some)

        // f |> Json.dsrlzStrngAtPath<FixtureNode> "data" |> buildFixtureRecord,
        // ((if isNull pred then "{'data':null}" else pred) |> Json.dsrlzStrngAtPath<PredictionNode option> "data"
        //   |> Option.map buildPredictionRecord))

    getPlayer = fun (PlayerId playerId) ->
      gc.Cypher
        .Match("(p:Player)")
        .Where(fun (p:PlayerNode) -> p.Id = playerId)
        .Return<PlayerNode>("p")
        .ResultsAsync.Result
      |> Seq.map buildPlayerRecord
      |> Seq.tryHead

    getPrivateLeague = fun (PrivateLeagueId leagueId) ->
      gc.Cypher
        .Match("(l:League)")
        .Where(fun (l:LeagueNode) -> l.Id = string leagueId)
        .Return<LeagueNode>("l")
        .ResultsAsync.Result
      |> Seq.map buildLeagueRecord
      |> Seq.tryHead

    getFixtureByTeams = fun (Team home) (Team away) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.HomeTeam = home && f.AwayTeam = away)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord
      |> Seq.head

    getMaxGameweekNo = fun () ->
      gc.Cypher
        .Match("(f:FixtureSet)")
        .Return<FixtureSetNode>("f")
        .ResultsAsync.Result
      |> Seq.map (fun f -> f.GameweekNo)
      |> Seq.sortDescending
      |> Seq.tryHead
      |> Option.map GameweekNo

    getPredictionsAggregate = fun (FixtureId fId) ->
      gc.Cypher
        .Match("(p:Prediction)-[:FOR_FIXTURE]->(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Return(fun () ->
          Return.As<decimal>("avg(p.HomeScore)"),
          Return.As<decimal>("avg(p.AwayScore)"),
          Return.As<int>("count(case p.HomeScore > p.AwayScore when true then 1 end"),
          Return.As<int>("count(case p.HomeScore < p.AwayScore when true then 1 end"),
          Return.As<int>("count(case p.HomeScore = p.AwayScore when true then 1 end"))
        .ResultsAsync.Result
      |> Seq.map (fun (avgHs, avgAs, hw, aw, dr) ->
          { AvgHomeScore = avgHs
            AvgAwayScore = avgAs
            HomeWinCount = hw
            AwayWinCount = aw
            DrawCount = dr })
      |> Seq.head

    getFixturesForTeam = fun (Team team) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.HomeTeam = team)
        .OrWhere(fun (f:FixtureNode) -> f.AwayTeam = team)
        .Return<FixtureNode>("f")
        .ResultsAsync.Result
      |> Seq.map buildFixtureRecord
  }

let nonQueries (gc:GraphClient) : NonQueries =
  { kickOffFixture = fun (FixtureId fId) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Set("f.HasKickedOff = true")
        .ExecuteWithoutResultsAsync().Wait()

    editFixtureKo = fun (FixtureId fId, KickOff ko) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Set("f.KickOff = $ko")
        .WithParam("ko", ko)
        .ExecuteWithoutResultsAsync().Wait()

    createFixtureSet = fun fs ->
      gc.Cypher
        .Create("(f:FixtureSet $param)")
        .WithParam("param", fs)
        .ExecuteWithoutResultsAsync().Wait()

    createFixture = fun (FixtureSetId fsId) (f:FixtureNode) ->
      gc.Cypher
        .Match("(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Create("(f:Fixture $param)-[:IN_FIXTURESET]->(fs)")
        .WithParam("param", f)
        .ExecuteWithoutResultsAsync().Wait()

    classifyFixture = fun (FixtureId fId, ScoreLine (Score homeScore, Score awayScore)) ->
      gc.Cypher
        .Match("(f:Fixture)")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Set("f.HasResult = true")
        .Set("f.HomeScore = $homeScore")
        .Set("f.AwayScore = $awayScore")
        .WithParam("homeScore", homeScore)
        .WithParam("awayScore", awayScore)
        .ExecuteWithoutResultsAsync().Wait()

    createPrediction =
      fun
        { PlayerId = PlayerId playerId
          FixtureId = FixtureId fId
          IsDoubleDown = isDdd
          ScoreLine = ScoreLine (Score home, Score away)
          Created = created
        } ->
        { PlayerId = playerId
          FixtureId = string fId
          Created = created
          IsDoubleDown = isDdd
          HomeScore = home
          AwayScore = away
        } |> fun p ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Match("(pl:Player)")
          .Where(fun (pl:PlayerNode) -> pl.Id = playerId)
          .Create("(pr:Prediction $param)-[:FOR_FIXTURE]->(f)")
          .Create("(pl)-[:PREDICTED]->(pr)")
          .WithParam("param", p)
          .ExecuteWithoutResultsAsync().Wait()

    deletePredictionSet = fun (PlayerId playerId, FixtureSetId fsId) ->
      gc.Cypher
        .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .AndWhere(fun (player:PlayerNode) -> player.Id = playerId)
        .Delete("pred")
        .ExecuteWithoutResultsAsync().Wait()

    deleteFixture = fun (FixtureId fId) ->
      gc.Cypher
        .Match("()-[r1:PREDICTED]->(pred:Prediction)-[r2:FOR_FIXTURE]->(f:Fixture)-[r3:IN_FIXTURESET]->()")
        .Where(fun (f:FixtureNode) -> f.Id = string fId)
        .Delete("r1, pred, r2, f, r3")
        .ExecuteWithoutResultsAsync().Wait()

    concludeFixtureSet = fun (FixtureSetId fsId) ->
      gc.Cypher
        .Match("(fs:FixtureSet)")
        .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
        .Set("fs.IsConcluded = true")
        .ExecuteWithoutResultsAsync().Wait()
  }