namespace Server.Application

open System

open Shared
open Shared.Routes
open Server
open Server.Commands
open Server.Queries
open Server.Utils
open Server.Infrastructure
open Server.Infrastructure.Push
open Config

open Microsoft.AspNetCore

module HttpHandlers =

  open Microsoft.AspNetCore.Http
  open Giraffe
  open Server.Elevated

  [<CLIMutable>]
  type FixtureSetHttp =
    { GameweekNo : int
      Fixtures : FixtureHttp list }
  and FixtureHttp =
    { FixtureId : Guid
      Home : string
      Away : string
      KickOff : DateTimeOffset }

  let createFixtureSet handleCommand next (ctx:HttpContext) =
    let respond next ctx (result:Rresult<Unit>) =
      match result with
      | Ok () -> Successful.CREATED "Created" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    let getFixtureSetFromRequest (ctx:HttpContext) =
      ctx.BindModelAsync<FixtureSetHttp>()
    let createFixtureSetCmd (fs:FixtureSetHttp) =
      fs.Fixtures
      |> List.map (fun f ->
          FixtureSetId (Guid.NewGuid())
          |> fun fsId ->
          { FixtureRecord.Id = FixtureId (Guid.NewGuid())
            FixtureSetId = fsId
            GameweekNo = GameweekNo fs.GameweekNo
            KickOff = KickOff f.KickOff
            TeamLine = TeamLine (Team f.Home, Team f.Away)
            ScoreLine = None
            SortOrder = 0
            HasKickedOff = false })
      |> fun fixtures -> GameweekNo fs.GameweekNo, fixtures
      |> CreateFixtureSet
      |> fun fscmd -> FixtureSetCommand (FixtureSetId (Guid.NewGuid()), fscmd)
    ctx
    |> (getFixtureSetFromRequest
    >> Task.toAsync
    >> Async.map createFixtureSetCmd
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))

  [<CLIMutable>]
  type EditFixtureKoHttp =
    { FixtureSetId : Guid
      FixtureId : Guid
      KickOff : DateTimeOffset }

  let editFixtureKo handleCommand next (ctx:HttpContext) =
    let respond next ctx (result:Rresult<Unit>) =
      match result with
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    let editFixtureKoCmd (e:EditFixtureKoHttp) =
      (FixtureId e.FixtureId, KickOff e.KickOff)
      |> EditFixtureKickOff
      |> fun fscmd -> FixtureSetCommand (FixtureSetId e.FixtureSetId, fscmd)
    ctx
    |> (fun ctx -> ctx.BindModelAsync<EditFixtureKoHttp>()
    >> Task.toAsync
    >> Async.map editFixtureKoCmd
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))

  [<CLIMutable>]
  type AppendFixtureHttp =
    { FixtureSetId : Guid
      Home : string
      Away : string
      KickOff : DateTimeOffset }

  let appendFixtureToGameweek handleCommand next (ctx:HttpContext) =
    let respond next ctx = function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    ctx
    |> (fun ctx -> ctx.BindModelAsync<AppendFixtureHttp>()
    >> Task.toAsync
    >> Async.map (fun e ->
      { Id = FixtureId (Guid.NewGuid())
        FixtureSetId = FixtureSetId e.FixtureSetId
        GameweekNo = GameweekNo 0
        KickOff = KickOff e.KickOff
        TeamLine = TeamLine (Team e.Home, Team e.Away)
        ScoreLine = None
        SortOrder = 0
        HasKickedOff = false }
      |> AppendFixture
      |> fun fscmd -> FixtureSetCommand (FixtureSetId e.FixtureSetId, fscmd))
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))

  [<CLIMutable>]
  type RemoveOpenFixtureHttp =
    { FixtureId : Guid
      FixtureSetId : Guid }

  let removeOpenFixture handleCommand next (ctx:HttpContext) =
    let respond next ctx = function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    ctx
    |> (fun ctx -> ctx.BindModelAsync<RemoveOpenFixtureHttp>()
    >> Task.toAsync
    >> Async.map (fun e ->
      FixtureId e.FixtureId
      |> RemoveOpenFixture
      |> fun fscmd -> FixtureSetCommand (FixtureSetId e.FixtureSetId, fscmd))
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))

  [<CLIMutable>]
  type FixtureClassificationHttp =
    { HomeTeam : string
      AwayTeam : string
      HomeScore : int
      AwayScore : int }

  let classifyFixture (deps:Dependencies) handleCommand next (ctx:HttpContext) =
    let respond next ctx (result:Rresult<Unit>) =
      match result with
      | Ok () -> Successful.CREATED "Created" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    let getClassificationFromRequest (ctx:HttpContext) =
      ctx.BindModelAsync<FixtureClassificationHttp>()
    let getFixtureFromRequest (fc:FixtureClassificationHttp) =
      deps.Queries.getFixtureByTeams (Team fc.HomeTeam) (Team fc.AwayTeam)
    let createClassifyFixtureCmd (fc:FixtureClassificationHttp, fr:FixtureRecord) =
      ClassifyFixture (fr.Id, ScoreLine (Score fc.HomeScore, Score fc.AwayScore))
      |> fun fscmd -> FixtureSetCommand (fr.FixtureSetId, fscmd)
    ctx
    |> (getClassificationFromRequest
    >> Task.toAsync
    >> Async.map (fun fc -> (fc, getFixtureFromRequest fc) |> createClassifyFixtureCmd)
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))

  let classifyAllFixtures (deps:Dependencies) handleCommand =
    BackgroundTasks.Classifier.classifyAllFixtures handleCommand deps
    Successful.OK "Ok"

  let classifyFixturesAfterGameweek (deps:Dependencies) handleCommand gwno =
    BackgroundTasks.Classifier.classifyFixturesAfterGameweek handleCommand deps (GameweekNo gwno)
    Successful.OK "Ok"

  [<CLIMutable>]
  type RemovePlayerHttp =
    { PlayerId : string }

  let removePlayer handleCommand =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<RemovePlayerHttp>()
      |> Task.map (fun p -> (PlayerId p.PlayerId, Remove) |> PlayerCommand)
      |> Task.bind (Async.toTask handleCommand)
      |> Task.bind (function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx)

  [<CLIMutable>]
  type AddPlayerToLeagueHttp =
    { PlayerId : string
      LeagueId : Guid }

  let addPlayerToLeague handleCommand =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<AddPlayerToLeagueHttp>()
      |> Task.map (fun p -> (PrivateLeagueId p.LeagueId, PlayerId p.PlayerId |> JoinLeague) |> PrivateLeagueCommand)
      |> Task.bind (Async.toTask handleCommand)
      |> Task.bind (function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx)

  [<CLIMutable>]
  type RemovePlayerFromLeagueHttp =
    { PlayerId : string
      LeagueId : Guid }

  let removePlayerFromLeague handleCommand =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<RemovePlayerFromLeagueHttp>()
      |> Task.map (fun p -> (PrivateLeagueId p.LeagueId, PlayerId p.PlayerId |> LeaveLeague) |> PrivateLeagueCommand)
      |> Task.bind (Async.toTask handleCommand)
      |> Task.bind (function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx)


  [<CLIMutable>]
  type RenameLeagueHttp =
    { LeagueName : string
      LeagueId : Guid }

  let renameLeague handleCommand =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<RenameLeagueHttp>()
      |> Task.map (fun r -> PrivateLeagueCommand(PrivateLeagueId r.LeagueId, RenameLeague (LeagueName r.LeagueName)))
      |> Task.bind (Async.toTask handleCommand)
      |> Task.bind (function
        | Ok () -> Successful.OK "Ok" next ctx
        | Error s -> ServerErrors.INTERNAL_ERROR s next ctx)


  [<CLIMutable>]
  type OverwritePredictionSetHttp =
    { SourcePlayerId : string
      DestinationPlayerId : string
      FixtureSetId : Guid }

  let overwritePredictionSet handleCommand =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<OverwritePredictionSetHttp>()
      |> Task.map (fun m ->
        PlayerId m.SourcePlayerId
        |> OverwritePredictionSet
        |> fun cmd -> PredictionSetCommand (PlayerId m.DestinationPlayerId, FixtureSetId m.FixtureSetId, cmd))
      |> Task.bind (Async.toTask handleCommand)
      |> Task.bind (function
      | Ok () -> Successful.OK "Ok" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx)

  [<CLIMutable>]
  type KickOffFixtureHttp =
    { HomeTeam : string
      AwayTeam : string
    }

  let kickOffFixture (deps:Dependencies) handleCommand next (ctx:HttpContext) =
    let respond next ctx (result:Rresult<Unit>) =
      match result with
      | Ok () -> Successful.CREATED "Created" next ctx
      | Error s -> ServerErrors.INTERNAL_ERROR s next ctx
    let getKoFromRequest (ctx:HttpContext) =
      ctx.BindModelAsync<KickOffFixtureHttp>()
    let getFixtureFromRequest (ko:KickOffFixtureHttp) =
      deps.Queries.getFixtureByTeams (Team ko.HomeTeam) (Team ko.AwayTeam)
    let createKoFixtureCmd (fr:FixtureRecord) =
      KickOffFixture fr.Id
      |> fun fscmd -> FixtureSetCommand (fr.FixtureSetId, fscmd)
    ctx
    |> (getKoFromRequest
    >> Task.toAsync
    >> Async.map (getFixtureFromRequest >> createKoFixtureCmd)
    >> Async.toTask (Async.bind handleCommand)
    >> Task.bind (respond next ctx))


  [<CLIMutable>]
  type TestNotifyHttp =
    { Title : string
      Body : string
      PlayerId : string }

  let getPlayerPushSubscriptions (deps:Dependencies) : List<PlayerId * PushSubscription> =
    Documents.repo deps.ElasticSearch
    |> fun repo -> repo.Read PlayerPushSubscriptions
    |> Option.defaultValue []
    |> List.distinct

  let testNotify (deps:Dependencies) =
    fun next (ctx:HttpContext) ->
      ctx.BindModelAsync<TestNotifyHttp>()
      |> Task.map (
          fun t ->
            getPlayerPushSubscriptions deps
            |> List.filter (fun (pId, _) -> pId = PlayerId t.PlayerId)
            |> List.iter (fun (_, ps) -> deps.PushNotify { PushMessage.Title = t.Title; Body = t.Body } ps)
            Successful.OK "Ok" next ctx)
      |> Task.toAsync
      |> Async.RunSynchronously


  let webApp (handleCommand:Command -> Ars<Unit>) (deps:Dependencies) =

    let appConfig = deps.ApplicationConfiguration

    let buildCsp (PlayerId id, PlayerName name) =
      { Jwt.JwtPlayer.sub = ""
        Jwt.JwtPlayer.name = name
        Jwt.JwtPlayer.roles = []
        Jwt.JwtPlayer.playerId = id }
      |> Jwt.jwtPlayerToAppToken appConfig.encryptionKey
      |> fun jwt -> { Token = jwt; Name = name; Id = PlayerId id }

    let cspToString : ClientSafePlayer -> string =
      Json.srlz >> (fun b -> b.ToArray()) >> Convert.ToBase64String >> Uri.EscapeDataString

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
        let respond (result:Rresult<string>) : HttpFuncResult =
          match result with
          | Ok s -> redirectTo false s next ctx
          | Error s ->
          ServerErrors.INTERNAL_ERROR s next ctx
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

    let printDocstore (deps:Dependencies) =
      Documents.repo deps.ElasticSearch
      |> fun repo -> repo.Print()
      |> string

    let runBackgroundMinuteTasks deps handleCommand =
      fun next (ctx:HttpContext) ->
        BackgroundTasks.minuteTasks |> List.iter(fun f -> f handleCommand deps)
        Successful.OK "Ok" next ctx

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
      POST >=> route  "/api/renameLeague" >=> renameLeague handleCommand
      POST >=> route  "/api/overwritePredictionSet" >=> overwritePredictionSet handleCommand
      POST >=> route  "/api/kickoffFixture" >=> kickOffFixture deps handleCommand
      POST >=> route  "/api/appendFixtureToGameweek" >=> appendFixtureToGameweek handleCommand
      POST >=> route  "/api/removeOpenFixture" >=> removeOpenFixture handleCommand
      POST >=> route  "/api/testNotify" >=> testNotify deps
      POST >=> route  "/api/bgMinuteTasks" >=> runBackgroundMinuteTasks deps handleCommand
      GET  >=> route  "/api/vapidPublicKey" >=> text appConfig.pushSubscriptionPublicKey
      GET  >=> route  "/api/printDocstore" >=> warbler (fun _ -> printDocstore deps |> text)
      GET  >=> route  "/api/now" >=> warbler (fun _ -> deps.Now().ToString("s") |> text)
      GET  >=> route  "/api/utcnow" >=> warbler (fun _ -> DateTime.UtcNow.ToString("s") |> text)
      Protocol.buildProtocol handleCommand deps
      htmlFile "public/index.html"
    ]
