namespace Server.Subscribers

open FSharp.Core
open Shared
open Server.Infrastructure

module PredictionCreatedSubscribers =

  let private createPrediction (deps:Dependencies) created (playerId, _, fId, scoreline) =
    { PlayerId = playerId
      FixtureId = fId
      IsDoubleDown = false
      ScoreLine = scoreline
      Created = created
    }
    |> deps.NonQueries.createPrediction

  let all =
    [ createPrediction
    ]

module PredictionEditCypher =

  let edit (deps:Dependencies) (PlayerId pId) (FixtureId fId) set =
    deps.Graph.Cypher
      .Match("(player:Player)-[:PREDICTED]->(p:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
      .Where(fun (player:PlayerNode) -> player.Id = pId)
      .AndWhere(fun (fixture:FixtureNode) -> fixture.Id = string fId)
      .Set(set)
      .ExecuteWithoutResultsAsync().Wait()

module PredictionIncHomeScoreSubscribers =

  let private incHomeScore (deps:Dependencies) _ (pId, fsId, fId) =
    PredictionEditCypher.edit deps pId fId "p.HomeScore = p.HomeScore + 1"

  let all =
    [ incHomeScore
    ]

module PredictionDecHomeScoreSubscribers =

  let private decHomeScore (deps:Dependencies) _ (pId, fsId, fId) =
    PredictionEditCypher.edit deps pId fId "p.HomeScore = p.HomeScore - 1"

  let all =
    [ decHomeScore
    ]

module PredictionIncAwayScoreSubscribers =

  let private incAwayScore (deps:Dependencies) _ (pId, fsId, fId) =
    PredictionEditCypher.edit deps pId fId "p.AwayScore = p.AwayScore + 1"

  let all =
    [ incAwayScore
    ]

module PredictionDecAwayScoreSubscribers =

  let private decAwayScore (deps:Dependencies) _ (pId, fsId, fId) =
    PredictionEditCypher.edit deps pId fId "p.AwayScore = p.AwayScore - 1"

  let all =
    [ decAwayScore
    ]

module PredictionDoubleDownAppliedSubscribers =

  let private applyDoubleDown (deps:Dependencies) _ (PlayerId pId, _, FixtureId fId) =
    deps.Graph.Cypher
      .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
      .Where(fun (player:PlayerNode) -> player.Id = pId)
      .AndWhere(fun (fixture:FixtureNode) -> fixture.Id = string fId)
      .Set("pred.IsDoubleDown = true")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ applyDoubleDown
    ]

module PredictionSetDoubleDownRemovedSubscribers =

  let private removeDoubleDown (deps:Dependencies) _ (PlayerId pId, FixtureSetId fsId) =
    deps.Graph.Cypher
      .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)-[:IN_FIXTURESET]->(fs:FixtureSet)")
      .Where(fun (player:PlayerNode) -> player.Id = pId)
      .AndWhere(fun (fs:FixtureSetNode) -> fs.Id = string fsId)
      .Set("pred.IsDoubleDown = false")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ removeDoubleDown
    ]

module PredictionSetOverwrittenSubscribers =

  let private overwritePredictionSet (deps:Dependencies) _ (sourcePlayerId, destinationPlayerId, fsId) =
    Documents.repo deps.ElasticSearch
    |> fun repo -> repo.Delete (PlayerFixtureSetsDocument destinationPlayerId)
    deps.NonQueries.deletePredictionSet (destinationPlayerId, fsId)
    deps.Queries.getPlayerFixtureSet sourcePlayerId fsId
    |> List.ofSeq
    |> List.map snd
    |> List.choose id
    |> List.map (fun p -> { p with PlayerId = destinationPlayerId })
    |> List.iter deps.NonQueries.createPrediction

  let all =
    [ overwritePredictionSet
    ]