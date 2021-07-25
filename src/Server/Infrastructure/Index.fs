namespace Server.Infrastructure

type Dependencies =
  { Graph : Neo4jClient.IGraphClient
    Queries : Server.Queries.Queries
    NonQueries : Server.Queries.NonQueries
    ElasticSearch : Unit
    PushNotify : Push.PushNotify }
