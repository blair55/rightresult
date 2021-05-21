namespace Server.Infrastructure

open System
open System.Text
open Shared
open Server.Elevated
open Server.Events
open Server.Queries
open Neo4jClient.Cypher

module Json =

  open Newtonsoft.Json
  open Newtonsoft.Json.Linq

  let private jsonConverter =
    Fable.JsonConverter() :> JsonConverter

  let srlzToString body =
    JsonConvert.SerializeObject(body, [|jsonConverter|])

  let srlz f =
    f |> (srlzToString >> Encoding.UTF8.GetBytes)

  let dsrlz<'a> (bytes:byte array) =
    let s = Encoding.UTF8.GetString (bytes)
    // printfn "type: %s / string: %s" typeof<'a>.Name s
    JsonConvert.DeserializeObject<'a> (s, [|jsonConverter|])

  let dsrlzStrng<'a> (s:string) =
    // printfn "type: %s / string: %s" typeof<'a>.Name s
    JsonConvert.DeserializeObject<'a> (s, [|jsonConverter|])

  let dsrlzStrngAtPath<'a> path (s:string) =
    JObject.Parse(s).[path].ToString() |> dsrlzStrng<'a>

  let dsrlzStrngAtPath2<'a> path1 path2 (s:string) =
    JObject.Parse(s).[path1].[path2].ToString() |> dsrlzStrng<'a>

module Graph =

  open Neo4jClient

  let client url =
    let gc = new GraphClient(Uri(url))//AuthTokens.Basic("username", "pasSW0rd"))
    gc.Connect(); gc

  let deleteAll (graph:IGraphClient) =
    graph.Cypher
      .Match("(n)")
      .DetachDelete("n")
      .ExecuteWithoutResults()

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
          .Results
        |> Seq.map buildFixtureRecord

      getAllFixtures = fun () ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Return<FixtureNode>("f")
          .Results
        |> Seq.map buildFixtureRecord

      getFixture = fun (FixtureId fId) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Return<FixtureNode>("f")
          .Results
        |> Seq.head
        |> buildFixtureRecord

      getFixturesInFixtureSet = fun (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Return<FixtureNode>("f")
          .Results
        |> Seq.map buildFixtureRecord

      getFixturesInLatestFixtureSet = fun () ->
        gc.Cypher
          .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .With("f, fs, fs.Id as fsId")
          .Return(fun f fsId -> f.CollectAs<FixtureNode>(), fsId.As<Guid>(), Return.As<int>("max(fs.GameweekNo)"))
          .Limit(Nullable<int>(1))
          .Results
        |> Seq.tryHead
        |> Option.map (fun (fixtures, fixtureSetId, maxGwno) ->
            FixtureSetId fixtureSetId, GameweekNo maxGwno, fixtures |> Seq.map buildFixtureRecord)

      getFixturesAwaitingResults = fun () ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.HasKickedOff = true)
          .AndWhere(fun (f:FixtureNode) -> f.HasResult = false)
          .Return<FixtureNode>("f")
          .Results
        |> Seq.map buildFixtureRecord

      getPlayerPredictionsByFixture = fun (PlayerId playerId) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .OptionalMatch("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f)")
          .Where(fun (player:PlayerNode) -> player.Id = playerId)
          .Return(fun f pred -> f.As<FixtureNode>(), pred.As<String>())
          .Results
        |> Seq.map (fun (f, predStr) ->
          buildFixtureRecord f, if predStr |> isNull then None
             else Json.dsrlzStrngAtPath<PredictionNode> "data" predStr |> buildPredictionRecord |> Some)

      getPlayerPredictionForFixture = fun (PlayerId playerId) (FixtureId fId) ->
        gc.Cypher
          .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f:Fixture)")
          .Where(fun (player:PlayerNode) -> player.Id = playerId)
          .AndWhere(fun (f:FixtureNode) -> f.Id = string fId)
          .Return<PredictionNode>("pred")
          .Results
        |> Seq.map buildPredictionRecord
        |> Seq.tryHead

      getFixtureSetEarliestKickOff = fun (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Return(fun () -> Return.As<DateTime>("min(f.KickOff)"))
          .Results
        |> Seq.head
        |> DateTimeOffset
        |> KickOff

      getPredictionsForPlayer = fun (PlayerId playerId) ->
        gc.Cypher
          .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
          .Where(fun (player:PlayerNode) -> player.Id = playerId)
          .Return(fun pred fixture -> fixture.As<FixtureNode>(), pred.As<PredictionNode>())
          .Results
        |> List.ofSeq
        |> List.map (fun (fixtureNode, predictionNode) ->
          buildFixtureRecord fixtureNode, buildPredictionRecord predictionNode)

      getPredictionsForPlayerInFixtureSet = fun (FixtureSetId fsId) (PlayerId playerId) ->
        gc.Cypher
          .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .AndWhere(fun (player:PlayerNode) -> player.Id = playerId)
          .Return(fun fixture pred -> fixture.As<FixtureNode>(), pred.As<PredictionNode>())
          .Results
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
          .Results
        |> List.ofSeq
        |> List.map (fun (fixtureNode, predictionNode) ->
          buildFixtureRecord fixtureNode, buildPredictionRecord predictionNode)

      getPlayersInPrivateLeague = fun (PrivateLeagueId privateLeagueId) ->
        gc.Cypher
          .Match("(p:Player)-[r:IN_LEAGUE]->(l:League)")
          .Where(fun (l:LeagueNode) -> l.Id = string privateLeagueId)
          .Return<PlayerNode>("p")
          .Results
        |> List.ofSeq
        |> List.map buildPlayerRecord

      getAllPlayers = fun () ->
        gc.Cypher
          .Match("(p:Player)")
          .Return<PlayerNode>("p")
          .Results
        |> Seq.map buildPlayerRecord
        |> List.ofSeq

      getFixtureSetGameweekNo = fun (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Return<FixtureSetNode>("fs")
          .Results
        |> Seq.head
        |> fun fs -> GameweekNo fs.GameweekNo

      getFixtureSetYearAndMonth = fun (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Return<FixtureSetNode>("fs")
          .Results
        |> Seq.head
        |> fun fs -> fs.Year, fs.Month

      getFixtureRecord = fun (FixtureId fId) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Return<FixtureNode>("f")
          .Results
        |> Seq.head
        |> buildFixtureRecord

      getUnconcludedFixtureSets = fun () ->
        gc.Cypher
          .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.IsConcluded = false)
          .Return(fun fs f -> fs.As<FixtureSetNode>(), f.CollectAs<FixtureNode>())
          .Results
        |> Seq.map (fun (fs, fixtures) ->
            fs.Id |> buildFixtureSetId,
            fs.GameweekNo |> GameweekNo,
            fixtures |> List.ofSeq |> List.map buildFixtureRecord)

      getPrivateLeagues = fun () ->
        gc.Cypher
          .Match("(l:League)")
          .Return<LeagueNode>("l")
          .Results
        |> Seq.map buildLeagueRecord

      getPrivateLeaguesAndMembers = fun () ->
        gc.Cypher
          .Match("(player:Player)-[:IN_LEAGUE]->(league:League)")
          .Return(fun player league -> league.As<LeagueNode>(), player.CollectAs<PlayerNode>())
          .Results
        |> Seq.map (fun (league, players) ->
          buildLeagueRecord league, players |> Seq.map buildPlayerRecord)

      getFixturesLength = fun () ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Return(fun () -> Return.As<int>("count(f)"))
          .Results
        |> Seq.exactlyOne

      getLeaguesPlayerIsIn = fun (PlayerId playerId) ->
        gc.Cypher
          .Match("(player)-[:IN_LEAGUE]->(league:League)")
          .Where(fun (player:PlayerNode) -> player.Id = playerId)
          .Return<LeagueNode>("league")
          .Results
        |> Seq.map buildLeagueRecord

      getPlayerFixtureSet = fun (PlayerId playerId) (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet {Id:{fsId}})")
          .OptionalMatch("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f)-[:IN_FIXTURESET]->(fs)")
          .WithParam("fsId", fsId)
          .Where(fun (player:PlayerNode) -> player.Id = playerId)
          .Return(fun f pred -> f.As<string>(), pred.As<string>())
          .Results
        |> Seq.map(fun (f, pred) ->
          f |> Json.dsrlzStrngAtPath<FixtureNode> "data" |> buildFixtureRecord,
          ((if isNull pred then "{'data':null}" else pred) |> Json.dsrlzStrngAtPath<PredictionNode option> "data"
            |> Option.map buildPredictionRecord))

      getPlayer = fun (PlayerId playerId) ->
        gc.Cypher
          .Match("(p:Player)")
          .Where(fun (p:PlayerNode) -> p.Id = playerId)
          .Return<PlayerNode>("p")
          .Results
        |> Seq.map buildPlayerRecord
        |> Seq.tryHead

      getPrivateLeague = fun (PrivateLeagueId leagueId) ->
        gc.Cypher
          .Match("(l:League)")
          .Where(fun (l:LeagueNode) -> l.Id = string leagueId)
          .Return<LeagueNode>("l")
          .Results
        |> Seq.map buildLeagueRecord
        |> Seq.tryHead

      getFixtureByTeams = fun (Team home) (Team away) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.HomeTeam = home && f.AwayTeam = away)
          .Return<FixtureNode>("f")
          .Results
        |> Seq.map buildFixtureRecord
        |> Seq.head

      getMaxGameweekNo = fun () ->
        gc.Cypher
          .Match("(f:FixtureSet)")
          .Return<FixtureSetNode>("f")
          .Results
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
          .Results
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
          .Results
        |> Seq.map buildFixtureRecord
    }

  let nonQueries (gc:GraphClient) : NonQueries =
    { kickOffFixture = fun (FixtureId fId) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Set("f.HasKickedOff = true")
          .ExecuteWithoutResults()

      editFixtureKo = fun (FixtureId fId, KickOff ko) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Set("f.KickOff = {ko}")
          .WithParam("ko", ko)
          .ExecuteWithoutResults()

      createFixtureSet = fun fs ->
        gc.Cypher
          .Create("(f:FixtureSet {param})")
          .WithParam("param", fs)
          .ExecuteWithoutResults()

      createFixture = fun (FixtureSetId fsId) (f:FixtureNode) ->
        gc.Cypher
          .Match("(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Create("(f:Fixture {param})-[:IN_FIXTURESET]->(fs)")
          .WithParam("param", f)
          .ExecuteWithoutResults()

      classifyFixture = fun (FixtureId fId, ScoreLine (Score homeScore, Score awayScore)) ->
        gc.Cypher
          .Match("(f:Fixture)")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Set("f.HasResult = true")
          .Set("f.HomeScore = {homeScore}")
          .Set("f.AwayScore = {awayScore}")
          .WithParam("homeScore", homeScore)
          .WithParam("awayScore", awayScore)
          .ExecuteWithoutResults()

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
            .Create("(pr:Prediction {param})-[:FOR_FIXTURE]->(f)")
            .Create("(pl)-[:PREDICTED]->(pr)")
            .WithParam("param", p)
            .ExecuteWithoutResults()

      deletePredictionSet = fun (PlayerId playerId, FixtureSetId fsId) ->
        gc.Cypher
          .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(f:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .AndWhere(fun (player:PlayerNode) -> player.Id = playerId)
          .Delete("pred")
          .ExecuteWithoutResults()

      deleteFixture = fun (FixtureId fId) ->
        gc.Cypher
          .Match("()-[r1:PREDICTED]->(pred:Prediction)-[r2:FOR_FIXTURE]->(f:Fixture)-[r3:IN_FIXTURESET]->()")
          .Where(fun (f:FixtureNode) -> f.Id = string fId)
          .Delete("r1, pred, r2, f, r3")
          .ExecuteWithoutResults()

      concludeFixtureSet = fun (FixtureSetId fsId) ->
        gc.Cypher
          .Match("(fs:FixtureSet)")
          .Where(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
          .Set("fs.IsConcluded = true")
          .ExecuteWithoutResults()
    }

module ElasticSearch =

  // open Elasticsearch.Net

  // let leagueIdToString = function
  //   | GlobalLeague -> Shared.Global.identifier
  //   | PrivateLeague (PrivateLeagueId leagueId) -> string leagueId

  // let leagueWindowToString = function
  //   | Full -> "full"
  //   | Week w -> sprintf "gw-%i" w
  //   | Month (y, m) -> sprintf "month-%i-%i" y m

  // let documentToIndexAndId doc =
  //   match doc with
  //   | LeagueTableDocument (leagueId, window) -> "LeagueTable", sprintf "%s-%s" (leagueIdToString leagueId) (leagueWindowToString window)
  //   | PlayerFixtureSetsDocument (PlayerId playerId) -> "PlayerFixtureSets", playerId
  //   | LeagueAllFixtureSetHistory leagueId -> "LeagueAllFixtureSetHistory", leagueIdToString leagueId
  //   | LeagueAllMonthHistory leagueId -> "LeagueAllMonthHistory", leagueIdToString leagueId
  //   | Matrix (leagueId, GameweekNo gwno) -> "Matrix", sprintf "%s-%i" (leagueIdToString leagueId) gwno
  //   |> fun (index, id) -> (index.ToLower(), id)

  let mutable _ds : Map<Document, obj> =
    Map.empty

  let private readDocument<'a> () document =
    _ds.TryFind document |> Option.map (fun a -> a :?> 'a)

  let private storeDocument<'a> () document (a:'a) =
    _ds <- _ds.Add(document, a :> obj)

  let private deleteDocument () document =
    _ds <- _ds.Remove(document)

  let private upsertDocument client document (init:'a) (f:'a -> 'a) =
    match readDocument client document with
    | Some d -> d
    | None -> init
    |> (f >> storeDocument client document)

  let private editDocument client document (f:'a -> 'a) =
    match readDocument client document with
    | Some d -> d |> (f >> storeDocument client document) |> Ok
    | None -> RemoteError.ServerSideError "Could not find document to edit" |> Result.Error

  type Repo<'d> =
    { Read : Document -> 'd option
      Edit : Document -> ('d -> 'd) -> Rresult<unit>
      Upsert : Document -> 'd -> ('d -> 'd) -> unit
      Insert : Document -> 'd -> unit
      Delete : Document -> unit
      Print : unit -> obj
    }

  let repo ds =
    { Read = readDocument ds
      Edit = editDocument ds
      Upsert = upsertDocument ds
      Insert = storeDocument ds
      Delete = deleteDocument ds
      Print = fun () -> _ds :> obj
    }

module EventStore =

  open EventStore.ClientAPI
  open EventStore.ClientAPI.SystemData
  open System.Threading.Tasks
  open Microsoft.FSharp.Reflection

  let eventStoreConnection uri un pw : IEventStoreConnection =
    let settings =
      ConnectionSettings.Create()
        // .UseConsoleLogger()
        .SetDefaultUserCredentials(new UserCredentials(un, pw))
        .Build()
    let connection =
      EventStoreConnection.Create(settings, Uri(uri))
    connection.ConnectAsync().Wait()
    printfn "Connected to event store"
    connection

  let toDatedEvent (e:ResolvedEvent) =
    DatedEvent(Json.dsrlz<Event> e.Event.Data, DateTimeOffset.FromUnixTimeMilliseconds(e.Event.CreatedEpoch))

  let readStreamEvents (store:IEventStoreConnection) (StreamId streamId) = async {
    try
      let! slice =
        store.ReadStreamEventsForwardAsync(streamId, int64 0, 4095, false)
        |> Task.toAsync
      let events =
        slice.Events
        |> Array.map toDatedEvent
        |> List.ofSeq
      return Ok (EventVersion slice.LastEventNumber, events)
    with ex ->
      return Result.Error (RemoteError.ServerSideError ex.Message) }

  let toEventStoreEvent (event:Event) =
    let case, _ =
      FSharpValue.GetUnionFields(event, typeof<Event>)
    EventData(Guid.NewGuid(), case.Name, true, Json.srlz event, [||])

  let private readAllEventsAndReturnLastPosition (store:IEventStoreConnection) onEvent =
    let rec inner (f:Position -> AllEventsSlice) p =
      f p
      |> fun slice ->
        printfn "reading from %i" slice.FromPosition.CommitPosition
        slice.NextPosition
        |> fun next ->
        slice.Events
        |> Array.filter (fun e -> not <| e.Event.EventStreamId.StartsWith("$"))
        |> Array.map toDatedEvent
        |> List.ofSeq
        |> List.iter onEvent
        if slice.IsEndOfStream
        then printfn "reading ended on %A" next.CommitPosition; next
        else inner f next
    inner
      (fun p -> store.ReadAllEventsForwardAsync(p, 1000, false).Result)
      (new Position())

  let private subscribeFromPosition (store:IEventStoreConnection) onEvent p =
    let position =
      new Nullable<Position>(p)
      // new Nullable<Int64>(0L)
    // let handleEvent (onEvent: IEventStoreConnection -> DatedEvent -> unit) (_:EventStoreCatchUpSubscription) (event:ResolvedEvent) =
    let handleEvent (onEvent: DatedEvent -> unit) (_:EventStoreCatchUpSubscription) (event:ResolvedEvent) =
      if event.Event.EventStreamId.StartsWith("$")
      then () // printfn "NOT HANDLING EVENT %s" event.Event.EventStreamId;
      else
        try toDatedEvent event |> onEvent
        with ex -> printfn "%A" ex
      Task.CompletedTask
    let eventAppeared =
      Func<EventStoreCatchUpSubscription, ResolvedEvent, Task> (handleEvent onEvent)
    let settings =
      CatchUpSubscriptionSettings.Default
      |> fun defaultSettings ->
      new CatchUpSubscriptionSettings (
        defaultSettings.MaxLiveQueueSize,
        defaultSettings.ReadBatchSize,
        verboseLogging = true,
        resolveLinkTos = defaultSettings.ResolveLinkTos,
        subscriptionName = defaultSettings.SubscriptionName)
    store.SubscribeToAllFrom(position, settings, eventAppeared)
    |> ignore

  let readFromBeginningAndSubscribeFromEnd store onEvent =
    readAllEventsAndReturnLastPosition store onEvent |> subscribeFromPosition store onEvent

  let store (connection:IEventStoreConnection) (StreamId streamId) (EventVersion expected) =
    try
      List.map toEventStoreEvent
      >> fun events -> connection.AppendToStreamAsync(streamId, expected, events).Wait()
      >> Ok
    with
      | :? EventStore.ClientAPI.Exceptions.WrongExpectedVersionException as ex ->
        fun _ -> WrongEventVersionError ex.Message |> Result.Error

module PushNotifications =

  type PushKeys =
    { Subject : string
      Public : string
      Private : string }

  type PushMessage =
    { Title : string
      Body : string }

  type PushNotify =
    PushMessage -> PushSubscription -> Unit

  let mutable semaphore =
    false

  let send (keys:PushKeys) (message:PushMessage) (push:PushSubscription) =
    if semaphore then
      try
        let client = WebPush.WebPushClient ()
        let vapidKeys = WebPush.VapidDetails (keys.Subject, keys.Public, keys.Private)
        let ps = new WebPush.PushSubscription (push.Endpoint, push.Keys.P256dh, push.Keys.Auth)
        client.SendNotification (ps, Json.srlzToString message, vapidKeys)
      with ex ->
        printfn "%s" ex.Message

module Persistence =

  type Dependencies =
    { Graph : Neo4jClient.IGraphClient
      Queries : Queries
      NonQueries : NonQueries
      ElasticSearch : Unit
      PushNotify : PushNotifications.PushNotify }

module Time =

  let private tz =
    TimeZoneInfo.FindSystemTimeZoneById("Europe/London")

  let toUkTime (date:DateTime) =
    tz.GetUtcOffset date
    |> date.Add
    |> fun d -> new DateTimeOffset(d)
