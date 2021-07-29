module Server.EntryPoint

open System

open Giraffe
open Server
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Server.Application
open Server.Infrastructure
open Server.Config


let compositionRoot () =
  let appConfig =
    buildAppConfig Environment.GetEnvironmentVariable

  let elasticSearch = () // DocumentStore Map.empty

  let eventStore =
    EventStore.eventStoreConnection appConfig.eventStoreUrl

  let graphClient = Graph.client appConfig.neo4jUrl
  let queries = Graph.queries graphClient
  let nonQueries = Graph.nonQueries graphClient
  // let now () = Time.toUkTime DateTime.UtcNow
  let now () = DateTime.Now

  let validateToken =
    Jwt.appTokenToJwtPlayer appConfig.encryptionKey
    >> Ok

  let pushNotify =
    Push.send
      { Subject = "https://rightresu.lt"
        Public = appConfig.pushSubscriptionPublicKey
        Private = appConfig.pushSubscriptionPrivateKey }

  let handleCommand =
    CommandHandler.handle (EventStore.readStreamEvents eventStore) (EventStore.store eventStore)

  let deps =
    { Now = now
      Graph = graphClient
      Queries = queries
      NonQueries = nonQueries
      EventStore = eventStore
      ElasticSearch = elasticSearch
      PushNotify = pushNotify
      ValidateToken = validateToken
      ApplicationConfiguration = appConfig
      FixtureSources = PremFixtures.fixturesSources }

  deps, handleCommand


[<EntryPoint>]
let main =
  function
  | [| "bgminute" as a |] ->
    printfn "running %s" a
    let deps, handleCommand = compositionRoot ()

    BackgroundTasks.minuteTasks
    |> List.iter (fun f -> f handleCommand deps)

    0

  | [| "bgdaily" as a |] ->
    printfn "running %s" a
    let deps, handleCommand = compositionRoot ()

    BackgroundTasks.dailyTasks
    |> List.iter (fun f -> f handleCommand deps)

    0

  | [| "generate" as a |] ->
    printfn "running! %s" a
    let deps, handleCommand = compositionRoot ()

    TestData.generate deps
    |> List.map (handleCommand >> Async.RunSynchronously)
    |> List.iter
         (function
         | Error e -> sprintf "%A" e |> failwith
         | _ -> ())
    |> printfn "Test data\n%A"

    0


  | _ ->

    printfn "web server"

    let configureApp (app: IApplicationBuilder) =
      let deps, handleCommand = compositionRoot ()
      Graph.deleteAll deps.Graph
      // ElasticSearch.setUpIndicies appConfig.elasticSearchUrl elasticSearch
      EventStore.subscribeToAll deps.EventStore (EventHandling.onEvent deps)
      Push.semaphore <- true

      app
        .UseStaticFiles()
        .UseGiraffe(HttpHandlers.webApp handleCommand deps)

    let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

    let port = 8085

    WebHost
      .CreateDefaultBuilder()
      .UseWebRoot("public")
      // .UseContentRoot("public")
      .Configure(
        Action<IApplicationBuilder> configureApp
      )
      .ConfigureServices(configureServices)
      .UseUrls(sprintf "http://0.0.0.0:%i/" port)
      .Build()
      .Run()

    0
