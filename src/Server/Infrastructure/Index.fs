namespace Server.Infrastructure

open System
open Shared

type Dependencies =
  { Now: Unit -> DateTime
    Graph : Neo4jClient.IGraphClient
    Queries : Server.Queries.Queries
    NonQueries : Server.Queries.NonQueries
    EventStore: EventStore.Client.EventStoreClient
    ElasticSearch : Unit
    PushNotify : Push.PushNotify
    ValidateToken: AppToken -> Result<Server.Jwt.JwtPlayer, RemoteError>
    ApplicationConfiguration: Server.Config.ApplicationConfiguration }
