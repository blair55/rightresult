namespace Server

open System
open System.IO
open System.Threading

open FSharp.Core
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open Shared
open Server.Login.Facebook
open Server.Elevated
open Server.Utils
open Server.Commands
open Server.Infrastructure
open Server.HttpHandlers
open Server.HttpHandlers.HttpHandlers
open Shared.Routes
open Persistence



module Server =

#if DEBUG
  let publicPath = "/Users/nblair/lab/rightresult/src/Client/public"
#else
  let publicPath = Path.GetFullPath "../Client/public"
#endif
  let port = 8085us

  type ApplicationConfiguration =
    { neo4jUrl : string
      eventStoreUrl : string
      eventStoreUsername : string
      eventStorePassword : string
      clientHost : string
      encryptionKey : Jwt.EncryptionSecretBase64
      facebookClientId : string
      facebookClientSecret : string
      twitterConsumerKey : string
      twitterConsumerSecret : string
      feedbackFilePath : string }

  let buildAppConfig (env:string -> string) =
    { neo4jUrl             = env "NEO4JURL"
      eventStoreUrl        = env "EVENTSTOREURL"
      eventStoreUsername   = env "EVENTSTOREUSERNAME"
      eventStorePassword   = env "EVENTSTOREPASSWORD"
      clientHost           = env "CLIENTHOST"
      encryptionKey        = env "ENCRYPTIONKEY" |> Jwt.EncryptionSecretBase64
      facebookClientId     = env "FACEBOOKCLIENTID"
      facebookClientSecret = env "FACEBOOKCLIENTSECRET"
      twitterConsumerKey   = env "TWITTERCONSUMERKEY"
      twitterConsumerSecret = env "TWITTERCONSUMERSECRET"
      feedbackFilePath     = env "FEEDBACKFILEPATH" }

  let buildProtocol handleCommand (deps:Dependencies) (config:ApplicationConfiguration) : HttpHandler =

    let q =
      deps.Queries


    let validateToken =
      Jwt.appTokenToJwtPlayer config.encryptionKey >> Ok

    let getFixturesForPlayer (from, size) (jwtPlayer:Jwt.JwtPlayer) : Map<FixtureId, FixturePredictionViewModel> =

      let now =
        DateTimeOffset.UtcNow

      let fixturesAndPredictions =
        q.getPlayerPredictionsByFixture (PlayerId jwtPlayer.playerId)
        |> List.ofSeq
        |> List.sortByDescending (fun (f, _) -> f.KickOff)
        |> List.skip from
        |> List.truncate size

      let isAnyDoubleDownFixtureForGameweekAlreadyKickedOff gwno =
        fixturesAndPredictions
        |> List.exists (fun (f, pred) ->
          match pred with
          | Some p when p.IsDoubleDown && f.GameweekNo = gwno && now > f.KickOff -> true
          | _ -> false)

      let getClassifiedInfo (pred:PredictionNode option) (result:ScoreLine) =
        pred
        |> Option.map (fun p -> ScoreLine (Score p.HomeScore, Score p.AwayScore), p.IsDoubleDown)
        |> Points.getPointsForPrediction result
        |> fun (points, category) -> result, points.Points, category
        |> FixtureState.Classified

      fixturesAndPredictions
      |> List.map (fun (f, pred) ->
        Guid.Parse f.Id |> FixtureId,
        { FixturePredictionViewModel.Id = Guid.Parse f.Id |> FixtureId
          FixtureSetId = Guid.Parse f.FixtureSetId |> FixtureSetId
          GameweekNo = GameweekNo f.GameweekNo
          SortOrder = f.SortOrder
          KickOff = KickOff f.KickOff
          FormattedKickOff = f.KickOff.Date.ToString("ddd MMM d, yyyy")
          TeamLine = TeamLine (Team f.HomeTeam, Team f.AwayTeam)
          State =
            match f.HasResult with
            | true -> ScoreLine (Score f.HomeScore, Score f.AwayScore) |> getClassifiedInfo pred
            | _ when now < f.KickOff -> FixtureState.Open
            | _ -> FixtureState.KickedOff
          Prediction = pred |> Option.map (fun p -> ScoreLine (Score p.HomeScore, Score p.AwayScore))
          IsDoubleDown = match pred with | Some p -> p.IsDoubleDown | _ -> false
          InProgress = false
          IsDoubleDownAvailable = not <| isAnyDoubleDownFixtureForGameweekAlreadyKickedOff f.GameweekNo })
      |> Map.ofSeq

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

    let makePrediction (appToken:AppToken) (PredictionAction (fsId, fId, team, vec)) =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        match team, vec with
        | Home, Inc -> SimplePredictionCommand IncHomeScore
        | Home, Dec -> SimplePredictionCommand DecHomeScore
        | Away, Inc -> SimplePredictionCommand IncAwayScore
        | Away, Dec -> SimplePredictionCommand DecAwayScore
        |> fun c ->
          DatedPredictionCommand (c, fId, PredictionEditDate DateTimeOffset.UtcNow)
          |> fun cmd -> PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, cmd)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn (PredictionAction (fsId, fId, team, vec))))

    let doDoubleDown (appToken:AppToken) (fsId, fId) =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        DatedPredictionCommand (DoubleDown, fId, PredictionEditDate DateTimeOffset.UtcNow)
        |> fun cmd -> PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, cmd)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn (fsId, fId)))

    let removeDownDown (appToken:AppToken) fsId =
      let buildCmd (jwtPlayer:Jwt.JwtPlayer) =
        PredictionSetCommand (PlayerId jwtPlayer.playerId, fsId, PredictionEditDate DateTimeOffset.UtcNow |> RemoveDoubleDown)
      appToken |> (
        validateToken
        >> Async.retn
        >> AsyncResult.map buildCmd
        >> AsyncResult.bind handleCommand
        >> AsyncResult.bind (fun _ -> AsyncResult.retn fsId))

    let submitFeedback (config:ApplicationConfiguration) feedback (jwtPlayer:Jwt.JwtPlayer) =
      let now = DateTime.UtcNow.ToString("s")
      sprintf "%s\nPLAYER: %s (%s)\n%s\n----------------\n\n" now jwtPlayer.name jwtPlayer.playerId feedback
      |> fun s -> File.AppendAllText(config.feedbackFilePath, s)

    let resultOfOption e = function
      | Some x -> Ok x
      | None -> Error e

    let getPlayerPointsTotal playerId : Rresult<PredictionPointsMonoid> =
      deps.ElasticSearch
      |> (ElasticSearch.repo
      >> fun repo -> repo.Read (LeagueTableDocument (GlobalLeague, Full))
      >> resultOfOption (ValidationError "League not found")
      >> Result.bind (fun (league:LeagueTableDoc) ->
        league.Members
        |> List.tryFind (fun (pId, _) -> pId = playerId)
        |> resultOfOption (ValidationError "Player not found"))
      >> Result.map (fun (_, m) -> m.Points))

    let getPlayerFixtureSets playerId =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (PlayerFixtureSetsDocument playerId)
      |> resultOfOption (ValidationError "Player not found")

    let getPlayerFixtureSet playerId fsId : Rresult<PlayerFixtureSetViewModel> =
      let fixturesAndPredictionsInFixtureSet =
        q.getPlayerFixtureSet playerId fsId
        |> List.ofSeq
      let fixtureSet =
        q.getFixtureSet fsId
      let now =
        DateTimeOffset.UtcNow
      // let sumPoints =
      if List.isEmpty fixturesAndPredictionsInFixtureSet then
        ValidationError "could not get playerfixture set" |> Error
      else
        match q.getPlayer playerId with
        | Some player ->
          { PlayerId = player.Id
            PlayerName = player.Name
            FixtureSetId = FixtureSetId (Guid.Parse fixtureSet.Id)
            GameweekNo = GameweekNo fixtureSet.GameweekNo
            AveragePoints = 0
            TotalPoints = PredictionPointsMonoid.Init
            Rows =
              fixturesAndPredictionsInFixtureSet
              |> List.filter (fun (f, _) -> KickOff.isLessThan f.KickOff now)
              |> List.map (fun (f, pred) ->
                let predictionDd =
                  if KickOff.isLessThan f.KickOff now then
                    Option.map (fun (p:PredictionRecord) -> p.ScoreLine, p.IsDoubleDown) pred
                  else None
                let resultAndPoints =
                  Option.map (fun sl -> sl, Points.getPointsForPrediction sl predictionDd) f.ScoreLine
                { PlayerFixtureSetKickedOffViewModelRow.FixtureId = f.Id
                  TeamLine = f.TeamLine
                  KickOff = f.KickOff
                  KickOffString = KickOff.groupFormat f.KickOff
                  SortOrder = f.SortOrder
                  Prediction = predictionDd
                  Points =
                    resultAndPoints
                    |> Option.map (fun (_, ppmcat) -> fst ppmcat)
                    |> function | Some p -> p | None -> PredictionPointsMonoid.Init
                  ResultAndPoints =
                    Option.map (fun (sl, ppmcat) -> sl, snd ppmcat) resultAndPoints
                })
              |> List.sortBy (fun p -> p.SortOrder)
          }
          |> fun vm -> { vm with TotalPoints = vm.Rows |> List.fold (fun acc r -> acc + r.Points) vm.TotalPoints }
          |> Ok
        | None ->
          ValidationError "could not get player" |> Error

    let getPrivateLeagueInfo leagueId =
      q.getPrivateLeague leagueId
      |> function
      | Some l -> Ok { LeagueName = l.LeagueName }
      | None -> ValidationError ("could not find league") |> Error

    let getLeagueWindow leagueId window : Rresult<LeagueTableDoc> =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueTableDocument (leagueId, window))
      |> resultOfOption (ValidationError "League not found")
      |> Result.map (fun t -> { t with Members = List.sortBy (fun (_, p) -> p.Position) t.Members })

    let getAllPlayers _ : PlayerViewModel list =
      q.getAllPlayers ()
      |> Seq.map (fun p -> { PlayerViewModel.Id = PlayerId p.Id; Name = PlayerName p.Name })
      |> List.ofSeq
      // |> List.sortWith (fun { Name = PlayerName a } { Name = PlayerName b } -> a.CompareTo b)
      |> List.sortBy (fun { Name = PlayerName a } -> a.ToLower())

    let getPlayerViewModel playerId =
      q.getPlayer playerId
      |> resultOfOption (ValidationError "Player not found")
      |> Result.bind (fun p -> Ok { PlayerViewModel.Id = p.Id; Name = p.Name })

    let getLeagueHistoryFixtureSets leagueId : Rresult<LeagueHistoryDoc> =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueAllFixtureSetHistory leagueId)
      |> resultOfOption (ValidationError "League fixture set history not found")

    let getLeagueHistoryMonths leagueId : Rresult<LeagueHistoryDoc> =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (LeagueAllMonthHistory leagueId)
      |> resultOfOption (ValidationError "League month history not found")

    let getLeagueMatrixForGameweek leagueId gwno : Rresult<MatrixDoc> =
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo -> repo.Read (Matrix (leagueId, gwno))
      |> resultOfOption (ValidationError "League matrix not found")

    let vt =
      validateToken

    let protocol =
      { getFixtures = fun fromSize t -> t |> (vt >> Result.map (getFixturesForPlayer fromSize) >> Async.retn)
        getFixturesLength = vt >> Result.map (fun _ -> q.getFixturesLength()) >> Async.retn
        getMaxGameweekNo = vt >> Result.map (fun _ -> q.getMaxGameweekNo() |> function | Some gwno -> gwno | None -> GameweekNo 1) >> Async.retn
        getPlayerLeagues = vt >> Result.map getLeaguesPlayerIsIn >> Async.retn
        getPrivateLeagueInfo = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getPrivateLeagueInfo leagueId) >> Async.retn)
        getLeagueTable = fun leagueId window t -> t |> (vt >> Result.bind (fun _ -> getLeagueWindow leagueId window) >> Async.retn)
        getAllPlayers = vt >> Result.map getAllPlayers >> Async.retn
        getPlayerInfo = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerViewModel playerId) >> Async.retn)
        getPlayerPointsTotal = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerPointsTotal playerId) >> Async.retn)
        getPlayerFixtureSets = fun playerId t -> t |> (vt >> Result.bind (fun _ -> getPlayerFixtureSets playerId) >> Async.retn)
        getPlayerFixtureSet = fun playerId fsId t -> t |> (vt >> Result.bind (fun _ -> getPlayerFixtureSet playerId fsId) >> Async.retn)
        getNewFixtureSet = vt >> (fun _ -> printfn "SFSDFSDFS"; FixtureSourcing.getNewFixtureSetViewModel deps) >> AsyncResult.retn
        getLeagueHistoryFixtureSets = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getLeagueHistoryFixtureSets leagueId) >> Async.retn)
        getLeagueHistoryMonths = fun leagueId t -> t |> (vt >> Result.bind (fun _ -> getLeagueHistoryMonths leagueId) >> Async.retn)
        getDateFormat = fun dateTime format t -> t |> (vt >> Result.map (fun _ -> dateTime.ToString(format)) >> Async.retn)
        getLeagueMatrix = fun leagueId gwno t -> t |> (vt >> Result.bind (fun _ -> getLeagueMatrixForGameweek leagueId gwno) >> Async.retn)
        submitFeedback = fun fb t -> t |> (vt >> Result.map (submitFeedback config fb) >> Async.retn)
        addNewFixtureSet = vt >> (fun _ -> FixtureSourcing.addNewFixtureSet deps) >> handleCommand
        prediction = makePrediction
        doubleDown = doDoubleDown
        removeDoubleDown = removeDownDown
        createLeague = createLeague
        joinLeague = joinLeague
        leaveLeague = leaveLeague
      }

    // FableGiraffeAdapter.httpHandlerWithBuilderFor protocol Routes.builder
    Remoting.createApi()
    |> Remoting.withRouteBuilder Routes.builder
    |> Remoting.fromValue protocol
    |> Remoting.buildHttpHandler

  let printDocstore (deps:Dependencies) =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo -> repo.Print()
    |> Giraffe.ResponseWriters.json

  let webApp handleCommand (deps:Dependencies) (appConfig:ApplicationConfiguration) =

    let buildCsp (PlayerId id, PlayerName name) =
      { Jwt.JwtPlayer.sub = ""
        Jwt.JwtPlayer.name = name
        Jwt.JwtPlayer.roles = []
        Jwt.JwtPlayer.playerId = id }
      |> Jwt.jwtPlayerToAppToken appConfig.encryptionKey
      |> fun jwt -> { Token = jwt; Name = name; Id = PlayerId id }

    let cspToString : ClientSafePlayer -> string =
      Json.srlz >> Convert.ToBase64String >> Uri.EscapeDataString

    let loginPlayer (playerId, playerName, email) =
      PlayerCommand (playerId, Login (playerName, email))
      |> handleCommand
      |> AsyncResult.map (fun () -> buildCsp (playerId, playerName))

    let authOk idPrefix (ext:ExternalAuth) : HttpHandler =
      fun next ctx ->
        let redirectPath =
          match ctx.Request.Cookies.TryGetValue(redirectPathKey) with
          | true, path -> path
          | _ -> ""
        ctx.Response.Cookies.Delete (redirectPathKey)
        let respond (result:Rresult<string>) =
          match result with
          | Ok s -> redirectTo false s next ctx
          | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
        let buildRedirectUrl =
          sprintf "%s/logged-in#player=%s" appConfig.clientHost
        let appendRedirectPath url =
          sprintf "%s&%s=%s" url redirectPathKey redirectPath
        (sprintf "%s-%s" idPrefix ext.id |> PlayerId, PlayerName ext.name, ext.email)
        |> (loginPlayer
        >> Async.toTask (AsyncResult.map (cspToString >> buildRedirectUrl >> appendRedirectPath))
        >> Task.bind respond)

    let writeRedirectPathCookie : HttpHandler =
      System.Threading.Thread.Sleep 2000
      fun next ctx ->
        ctx.Response.Cookies.Append(redirectPathKey, HttpContext.requestFormKey ctx redirectPathKey)
        next ctx

    // let removeRedirectPathCookie : HttpHandler =
    //   fun next ctx ->
    //     ctx.Response.Cookies.Delete (redirectPathKey)
    //     next ctx

    let facebookConfig : Login.Facebook.Configuration =
      { clientId = appConfig.facebookClientId
        clientSecret = appConfig.facebookClientSecret
        baseUrl = appConfig.clientHost
        middleWare = writeRedirectPathCookie
        authError = ServerErrors.INTERNAL_ERROR
        authOk = authOk "fb" }

    let twitterConfig : Login.Twitter.Configuration =
      { consumerKey = appConfig.twitterConsumerKey
        consumerSecret = appConfig.twitterConsumerSecret
        baseUrl = appConfig.clientHost
        middleWare = writeRedirectPathCookie
        authError = ServerErrors.INTERNAL_ERROR
        authOk = authOk "tw" }

    choose [
      Login.Facebook.handler facebookConfig
      Login.Twitter.handler twitterConfig
      POST >=> route  "/api/fixtureKo" >=> editFixtureKo handleCommand
      POST >=> route  "/api/fixtureSet" >=> createFixtureSet handleCommand
      POST >=> route  "/api/removePlayer" >=> removePlayer handleCommand
      POST >=> route  "/api/classifyFixture" >=> classifyFixture deps handleCommand
      POST >=> route  "/api/classifyAllFixtures" >=> classifyAllFixtures deps handleCommand
      POST >=> routef "/api/classifyFixturesAfterGameweek/%i" (classifyFixturesAfterGameweek deps handleCommand)
      POST >=> route  "/api/addPlayerToLeague" >=> addPlayerToLeague handleCommand
      POST >=> route  "/api/removePlayerFromLeague" >=> removePlayerFromLeague handleCommand
      POST >=> route  "/api/overwritePredictionSet" >=> overwritePredictionSet handleCommand
      POST >=> route  "/api/kickoffFixture" >=> kickOffFixture deps handleCommand
      GET  >=> route  "/api/printDocstore" >=> printDocstore deps
      buildProtocol handleCommand deps appConfig
      htmlFile "index.html"
    ]

  let inf =
    let appConfig =
      buildAppConfig Environment.GetEnvironmentVariable
    let elasticSearch = ()
      // DocumentStore Map.empty
    let eventStore =
      EventStore.eventStoreConnection appConfig.eventStoreUrl appConfig.eventStoreUsername appConfig.eventStorePassword
    let graphClient =
      Graph.client appConfig.neo4jUrl
    let queries =
      Graph.queries graphClient
    let nonQueries =
      Graph.nonQueries graphClient
    let handleCommand =
      CommandHandler.handle
        (EventStore.readStreamEvents eventStore)
        (EventStore.store eventStore)
    appConfig, elasticSearch, eventStore, graphClient, queries, nonQueries, handleCommand

  type RecurringTasks () =
    interface IHostedService with
      member __.StartAsync ct =
        printfn "STARTING RECURRING TASKS"
        let (_, _, _, _, queries, _, handleCommand) = inf
        async {
          while not ct.IsCancellationRequested do
            Whistler.kickOffFixtures handleCommand queries DateTimeOffset.Now
            Classifier.classifyKickedOffFixtures handleCommand queries
            return! Async.Sleep 60000
        } |> Async.StartAsTask :> Tasks.Task
      member __.StopAsync _ =
        printfn "STOPPING RECURRING TASKS"
        Tasks.Task.CompletedTask

  let configureApp (app:IApplicationBuilder) =
    let (appConfig, elasticSearch, eventStore, graphClient, queries, nonQueries, handleCommand) =
      inf
    { Dependencies.Graph = graphClient; Queries = queries; NonQueries = nonQueries; ElasticSearch = elasticSearch }
    |> fun deps ->
    EventHandling.onEvent deps
    |> fun onEvent ->
    Graph.deleteAll graphClient
    // ElasticSearch.setUpIndicies appConfig.elasticSearchUrl elasticSearch
    EventStore.readFromBeginningAndSubscribeFromEnd eventStore onEvent
    app
      .UseStaticFiles()
      .UseGiraffe(webApp handleCommand deps appConfig)

  let configureServices (services:IServiceCollection) =
    services
#if DEBUG
#else
      .AddSingleton<IHostedService, RecurringTasks>()
#endif
      .AddGiraffe() |> ignore

  WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls(sprintf "http://0.0.0.0:%i/" port)
    .Build()
    .Run()
