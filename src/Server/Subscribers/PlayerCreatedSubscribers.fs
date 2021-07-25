namespace Server.Subscribers

open Shared
open FSharp.Core
open Server.Infrastructure

module PlayerCreatedSubscribers =

  let private playersInGraph (deps:Dependencies) created (PlayerId id, (PlayerName name), email) =
    { PlayerNode.Id = id
      Created = created
      Name = name
      Email = email
      LastLogin = created }
    |> fun p ->
      deps.Graph.Cypher
        .Create("(p:Player $param)")
        .WithParam("param", p)
        .ExecuteWithoutResultsAsync().Wait()

  let private addToGlobalLeague (deps:Dependencies) created (playerId, name, email) =
    let repo =
      Documents.repo deps.ElasticSearch
    repo.Upsert
      (LeagueTableDocument (GlobalLeague, Full))
      (LeagueTableDoc.Init Global.leagueName)
      (fun table -> { table with Members = (playerId, LeagueTableMember.Init name)::table.Members })

  let all =
    [ playersInGraph
      addToGlobalLeague
    ]

module PlayerLoggedInSubscribers =

  let private playerLoggedIn (deps:Dependencies) created (PlayerId id) =
    deps.Graph.Cypher
      .Match("(p:Player)")
      .Where(fun (p:PlayerNode) -> p.Id = id)
      .Set("p.LastLogin = $Created")
      .WithParam("Created", created)
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ playerLoggedIn
    ]

module PlayerRemovedSubscribers =

  let private removePlayerFromGraph (deps:Dependencies) created (PlayerId pId) =
    deps.Graph.Cypher
      .Match("(p:Player)")
      .Where(fun (p:PlayerNode) -> p.Id = pId)
      .DetachDelete("p")
      .ExecuteWithoutResultsAsync().Wait()

  let private removePlayerFromFullTables (deps:Dependencies) created playerId =
    let repo =
      Documents.repo deps.ElasticSearch
    deps.Queries.getPrivateLeagues ()
      |> List.ofSeq
      |> List.map (fun l -> PrivateLeague l.PrivateLeagueId)
      |> fun leagueIds -> GlobalLeague :: leagueIds
      |> List.map (fun leagueId ->
          repo.Edit
            (LeagueTableDocument (leagueId, Full))
            (fun table -> { table with Members = table.Members |> List.filter (fun (pId, _) -> pId <> playerId) }))
      |> ignore

  let all =
    [ removePlayerFromGraph
      removePlayerFromFullTables
    ]

module PlayerSubscribedToPushSubscribers =

  open Server.Infrastructure.Push

  let private saveSubscriptionDoc (deps:Dependencies) created (playerId, subscription) =
    let repo =
      Documents.repo deps.ElasticSearch
    repo.Upsert
      PlayerPushSubscriptions
      []
      (fun l -> (playerId, subscription) :: l)
    deps.PushNotify
      { PushMessage.Title = "Heads up!"
        Body = "Notifications are working" }
      subscription

  let private saveSubscriptionGraph (deps:Dependencies) created (PlayerId playerId, subscription:PushSubscription) =
    deps.Graph.Cypher
      .Match("(p:Player)")
      .Where(fun (p:PlayerNode) -> p.Id = playerId)
      .Create("(p)-[:SUBSCRIBED $rel]->(s:Subscription $sub)")
      .WithParam("sub", dict [ "auth", subscription.Endpoint ])
      .WithParam("rel", dict [ "Subscribed", created ])
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ saveSubscriptionDoc
      saveSubscriptionGraph
    ]
