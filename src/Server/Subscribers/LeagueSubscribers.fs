namespace Server.Subscribers

open FSharp.Core
open Shared
open Server.Infrastructure
open Persistence

module CreateLeagueSubscribers =

  let private createLeagueGraph (deps:Dependencies) created (PrivateLeagueId leagueId, LeagueName leagueName, _) =
    { Id = string leagueId
      Created = created
      Name = leagueName } |> fun l ->
      deps.Graph.Cypher
        .Create("(l:League {param})")
        .WithParam("param", l)
        .ExecuteWithoutResults()

  let private createLeagueLatestTableDoc (deps:Dependencies) created (leagueId, leagueName, _) =
    ElasticSearch.repo deps.ElasticSearch
    |> fun repo ->
      repo.Insert
        (LeagueTableDocument (PrivateLeague leagueId, Full))
        (LeagueTableDoc.Init leagueName)

  let all =
    [ createLeagueGraph
      createLeagueLatestTableDoc ]

module LeagueJoinedSubscribers =

  let private joinLeagueGraph (deps:Dependencies) created (PrivateLeagueId leagueId, PlayerId playerId) =
    deps.Graph.Cypher
      .Match("(p:Player)", "(l:League)")
      .Where(fun (p:PlayerNode) -> p.Id = playerId)
      .AndWhere(fun (l:LeagueNode) -> l.Id = string leagueId)
      .Create("(p)-[:IN_LEAGUE {r}]->(l)")
      .WithParam("r", dict [ "Joined", created ])
      .ExecuteWithoutResults()

  let private joinLeagueUpdateLatestTableDoc (deps:Dependencies) created (leagueId, playerId) =
    match deps.Queries.getPlayer playerId with
    | Some p ->
      ElasticSearch.repo deps.ElasticSearch
      |> fun repo ->
        repo.Edit
          (LeagueTableDocument (PrivateLeague leagueId, Full))
          (fun l -> { l with Members = (playerId, LeagueTableMember.Init p.Name)::l.Members })
        |> ignore
    | None -> ()

  let all =
    [ joinLeagueGraph
      joinLeagueUpdateLatestTableDoc ]

module LeagueLeftSubscribers =

  let private leaveLeague (deps:Dependencies) created (PrivateLeagueId leagueId, PlayerId playerId) =
    deps.Graph.Cypher
      // .Match("(p:Player)", "(l:League)")
      .Match("(p:Player)-[r:IN_LEAGUE]->(l:League)")
      .Where(fun (p:PlayerNode) -> p.Id = playerId)
      .AndWhere(fun (l:LeagueNode) -> l.Id = string leagueId)
      // .Match("(p)-[r:IN_LEAGUE]->(l)")
      .Delete("r")
      .ExecuteWithoutResults()

  let all =
    [ leaveLeague ]

module LeagueRemovedSubscribers =

  let private removeLeague (deps:Dependencies) created (PrivateLeagueId leagueId) =
    deps.Graph.Cypher
      .OptionalMatch("()-[r]->(league:League)")
      .Where(fun (l:LeagueNode) -> l.Id = string leagueId)
      .Delete("r, league")
      .ExecuteWithoutResults()

  let all =
    [ removeLeague ]
