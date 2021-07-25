﻿namespace Server

open System

open Giraffe
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Server.Application.HttpHandlers
open Server.Infrastructure

module Server =

  let now () =
    DateTime.UtcNow
    |> Time.toUkTime

  let inf =
    let appConfig =
      Config.buildAppConfig Environment.GetEnvironmentVariable
    let elasticSearch = ()
      // DocumentStore Map.empty
    let eventStore =
      // EventStore.eventStoreConnection appConfig.eventStoreUrl appConfig.eventStoreUsername appConfig.eventStorePassword
      EventStore.eventStoreConnection appConfig.eventStoreUrl
    let graphClient =
      Graph.client appConfig.neo4jUrl
    let queries =
      Graph.queries graphClient
    let nonQueries =
      Graph.nonQueries graphClient
    let pushNotify =
      Push.send
        { Subject = "https://rightresu.lt"
          Public = appConfig.pushSubscriptionPublicKey
          Private = appConfig.pushSubscriptionPrivateKey }
    let handleCommand =
      CommandHandler.handle
        (EventStore.readStreamEvents eventStore)
        (EventStore.store eventStore)
    appConfig, elasticSearch, eventStore, graphClient, queries, nonQueries, pushNotify, handleCommand


  let configureApp (app:IApplicationBuilder) =
    let (appConfig, elasticSearch, eventStore, graphClient, queries, nonQueries, pushNotify, handleCommand) =
      inf
    { Graph = graphClient
      Queries = queries
      NonQueries = nonQueries
      ElasticSearch = elasticSearch
      PushNotify = pushNotify }
    |> fun deps ->
    EventHandling.onEvent deps
    |> fun onEvent ->
    Graph.deleteAll graphClient
    // ElasticSearch.setUpIndicies appConfig.elasticSearchUrl elasticSearch
    // EventStore.readFromBeginningAndSubscribeFromEnd eventStore onEvent
    EventStore.subscribeToAll eventStore onEvent |> Json.srlzToString |> printfn "subscription: %s"
    Push.semaphore <- true
    app
      .UseStaticFiles()
      .UseGiraffe(webApp handleCommand deps appConfig now)

  let configureServices (services:IServiceCollection) =
    services
#if DEBUG
#else
      .AddHostedService<RecurringTasks>()
#endif
      .AddGiraffe() |> ignore

  let port = 8085
  // let publicPath = Path.Combine(Directory.GetCurrentDirectory(), "public")

  WebHost
    .CreateDefaultBuilder()
    .UseWebRoot("public")
    // .UseContentRoot("public")
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls(sprintf "http://0.0.0.0:%i/" port)
    .Build()
    .Run()
