namespace Server.Subscribers

open FSharp.Core
open Shared
open Server.Infrastructure

module CreateLeagueSubscribers =

  let private createLeagueGraph (deps:Dependencies) created (PrivateLeagueId leagueId, LeagueName leagueName, _) =
    { Id = string leagueId
      Created = created
      Name = leagueName } |> fun l ->
      deps.Graph.Cypher
        .Create("(l:League $param)")
        .WithParam("param", l)
        .ExecuteWithoutResultsAsync().Wait()

  let private createLeagueLatestTableDoc (deps:Dependencies) created (leagueId, leagueName, _) =
    Documents.repo deps.ElasticSearch
    |> fun repo ->
      repo.Insert
        (LeagueTableDocument (PrivateLeague leagueId, Full))
        (LeagueTableDoc.Init leagueName)

  let private createLeagueMatrixDoc (deps:Dependencies) created (leagueId, leagueName, _) =
    deps.Queries.getAllFixtureSetsAndFixtures ()
    |> List.iter (fun (fsId, gwno, fixtures) ->
      let columns =
        fixtures
        |> List.ofSeq
        |> List.map (fun f ->
          f.Id,
          { MatrixFixture.TeamLine = f.TeamLine
            KickOff = f.KickOff
            State = MatrixFixtureState.Open
            SortOrder = f.SortOrder })
        |> Map.ofList
      Documents.repo deps.ElasticSearch
      |> fun repo ->
          { FixtureSetId = fsId
            LeagueName = leagueName
            LeagueId = PrivateLeague leagueId
            GameweekNo = gwno
            Columns = columns
            Rows = Map.empty }
          |> repo.Insert (Matrix (PrivateLeague leagueId, gwno)))

  let all =
    [ createLeagueGraph
      createLeagueLatestTableDoc
      createLeagueMatrixDoc ]


module LeagueRenamedSubscribers =

  let private renameLeagueGraph (deps:Dependencies) (PrivateLeagueId leagueId, LeagueName leagueName) =
    deps.Graph.Cypher
      .Match("(l:League)")
      .Where(fun (l:LeagueNode) -> l.Id = string leagueId)
      .Set("l.Name = $name")
      .WithParam("name", leagueName)
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ renameLeagueGraph ]

module LeagueJoinedSubscribers =

  let private joinLeagueGraph (deps:Dependencies) created (PrivateLeagueId leagueId, PlayerId playerId) =
    deps.Graph.Cypher
      .Match("(p:Player)", "(l:League)")
      .Where(fun (p:PlayerNode) -> p.Id = playerId)
      .AndWhere(fun (l:LeagueNode) -> l.Id = string leagueId)
      .Merge("(p)-[r:IN_LEAGUE]->(l)")
      // .CreateUnique("(p)-[:IN_LEAGUE {r}]->(l)")
      // .WithParam("r", dict [ "Joined", created ])
      .ExecuteWithoutResultsAsync().Wait()

  let private joinLeagueUpdateLatestTableDoc (deps:Dependencies) created (leagueId, playerId) =
    match deps.Queries.getPlayer playerId with
    | Some p ->
      Documents.repo deps.ElasticSearch
      |> fun repo ->
        let members league =
          if List.exists (fun (pId, _) -> pId = playerId) league.Members
          then league
          else { league with Members = (playerId, LeagueTableMember.Init p.Name)::league.Members }
        repo.Edit
          (LeagueTableDocument (PrivateLeague leagueId, Full))
          members
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
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ leaveLeague ]

module LeagueRemovedSubscribers =

  let private removeLeague (deps:Dependencies) created (PrivateLeagueId leagueId) =
    deps.Graph.Cypher
      .OptionalMatch("()-[r]->(league:League)")
      .Where(fun (l:LeagueNode) -> l.Id = string leagueId)
      .Delete("r, league")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ removeLeague ]
