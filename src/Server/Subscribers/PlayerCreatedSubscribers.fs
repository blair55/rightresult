namespace Server.Subscribers

open FSharp.Core
open Shared
open Server.Infrastructure
open Persistence
open System

module PlayerCreatedSubscribers =

  let private playersInGraph (deps:Dependencies) created (PlayerId id, (PlayerName name), email) =
    { PlayerNode.Id = id
      Created = created
      Name = name
      Email = email
      LastLogin = created }
    |> fun p ->
      deps.Graph.Cypher
        .Create("(p:Player {param})")
        .WithParam("param", p)
        .ExecuteWithoutResults()

  let private addToGlobalLeague (deps:Dependencies) created (playerId, name, email) =
    let repo =
      ElasticSearch.repo deps.ElasticSearch
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
      .Set("p.LastLogin = {Created}")
      .WithParam("Created", created)
      .ExecuteWithoutResults()

  let all =
    [ playerLoggedIn
    ]

module PlayerRemovedSubscribers =

  let private removePlayerFromGraph (deps:Dependencies) created (PlayerId pId) =
    deps.Graph.Cypher
      .Match("(p:Player)")
      .Where(fun (p:PlayerNode) -> p.Id = pId)
      .DetachDelete("p")
      .ExecuteWithoutResults()

  let private removePlayerFromFullTables (deps:Dependencies) created playerId =
    let repo =
      ElasticSearch.repo deps.ElasticSearch
    deps.Queries.getPrivateLeagues ()
      |> List.ofSeq
      |> List.map (fun l -> Guid.Parse l.Id |> PrivateLeagueId |> PrivateLeague)
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
