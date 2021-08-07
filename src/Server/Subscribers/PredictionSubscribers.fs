namespace Server.Subscribers

open FSharp.Core
open Shared
open Server.Infrastructure

module PredictionCreatedSubscribers =

  let private createPrediction (deps:Dependencies) created (playerId, _, fId, scoreline) =
    { PlayerId = playerId
      FixtureId = fId
      Modifier = PredictionModifier.None
      ScoreLine = scoreline
      Created = created
    }
    |> deps.NonQueries.createPrediction

  let all =
    [ createPrediction
    ]

module PredictionEditCypher =

  let edit (deps:Dependencies) (PlayerId pId) (FixtureId fId) =
    deps.Graph.Cypher
      .Match("(player:Player)-[:PREDICTED]->(p:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
      .Where(fun (player:PlayerNode) -> player.Id = pId)
      .AndWhere(fun (fixture:FixtureNode) -> fixture.Id = string fId)

module PredictionIncHomeScoreSubscribers =

  let private incHomeScore (deps:Dependencies) _ (pId, fsId, fId) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.HomeScore = p.HomeScore + 1")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ incHomeScore
    ]

module PredictionDecHomeScoreSubscribers =

  let private decHomeScore (deps:Dependencies) _ (pId, fsId, fId) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.HomeScore = p.HomeScore - 1")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ decHomeScore
    ]

module PredictionIncAwayScoreSubscribers =

  let private incAwayScore (deps:Dependencies) _ (pId, fsId, fId) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.AwayScore = p.AwayScore + 1")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ incAwayScore
    ]

module PredictionDecAwayScoreSubscribers =

  let private decAwayScore (deps:Dependencies) _ (pId, fsId, fId) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.AwayScore = p.AwayScore - 1")
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ decAwayScore
    ]

module PredictionSetHomeScoreSubscribers =

  let private setHomeScore (deps:Dependencies) pId fId (Score score) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.HomeScore = $HomeScore")
      .WithParam("HomeScore", score)
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ setHomeScore
    ]

module PredictionSetAwayScoreSubscribers =

  let private setAwayScore (deps:Dependencies) pId fId (Score score) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.AwayScore = $AwayScore")
      .WithParam("AwayScore", score)
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ setAwayScore
    ]

module PredictionSetScoreSubscribers =

  let private setScore (deps:Dependencies) pId fId (ScoreLine (Score home, Score away)) =
    (PredictionEditCypher.edit deps pId fId)
      .Set("p.HomeScore = $HomeScore")
      .Set("p.AwayScore = $AwayScore")
      .WithParam("HomeScore", home)
      .WithParam("AwayScore", away)
      .ExecuteWithoutResultsAsync().Wait()

  let all =
    [ setScore
    ]

module UpdatePredictionModifier =

  let setModifier modifier (deps:Dependencies) (PlayerId pId, FixtureId fId) =
    deps.Graph.Cypher
      .Match("(player:Player)-[:PREDICTED]->(pred:Prediction)-[:FOR_FIXTURE]->(fixture:Fixture)")
      .Where(fun (player:PlayerNode) -> player.Id = pId)
      .AndWhere(fun (fixture:FixtureNode) -> fixture.Id = string fId)
      .Set("pred.Modifier = $modifier")
      .WithParam("modifier", modifier)
      .ExecuteWithoutResultsAsync().Wait()

module PredictionBigUpAppliedSubscribers =

  let all =
    [ UpdatePredictionModifier.setModifier PredictionModifier.Consts.BigUp
    ]

module PredictionDoubleDownAppliedSubscribers =

  let all =
    [ UpdatePredictionModifier.setModifier PredictionModifier.Consts.DoubleDown
    ]

module PredictionDoubleDownRemovedSubscribers =

  let all =
    [ UpdatePredictionModifier.setModifier PredictionModifier.Consts.None
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