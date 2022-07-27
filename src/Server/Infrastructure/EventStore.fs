module Server.Infrastructure.EventStore

open System
open System.Text
open Shared
open Server.Elevated
open Server.Events
open EventStore.Client
open System.Threading.Tasks
open Microsoft.FSharp.Reflection
open System.Linq

let eventStoreConnection url  =
  new EventStoreClient(EventStoreClientSettings.Create url)

let private toDatedEvent (e:ResolvedEvent) =
  DatedEvent(Json.dsrlz<Event> e.Event.Data, e.Event.Created)

let readStreamEvents (client:EventStoreClient) (StreamId streamId) = async {
  try
    let response = client.ReadStreamAsync(Direction.Forwards, streamId, StreamPosition.Start)
    let! state = response.ReadState |> Task.toAsync
    if state = ReadState.StreamNotFound
    then return (Ok [])
    else
      let! slice = response.ToListAsync().AsTask() |> Task.toAsync
      let events = slice |> List.ofSeq |> List.map toDatedEvent
      return Ok events
  with ex ->
    return Result.Error (RemoteError.ServerSideError ex.Message) }

let toEventStoreEvent (event:Event) =
  let case, _ = FSharpValue.GetUnionFields(event, typeof<Event>)
  EventData(Uuid.NewUuid(), case.Name, Json.srlz event)

let store (client:EventStoreClient) (StreamId streamId) (EventVersion expected) events =
  try
    events
      |> List.map toEventStoreEvent
      |> fun events -> client.AppendToStreamAsync(streamId, StreamRevision expected, events)
      |> Async.AwaitTask
      |> Async.map (ignore >> Ok)
  with
    | :? WrongExpectedVersionException as ex -> WrongEventVersionError ex.Message |> Error |> Async.retn

let subscribeToAll (client:EventStoreClient) onEvent =
  client.SubscribeToAllAsync(
    FromAll.Start,
    (fun _ (e:ResolvedEvent) _ ->
      try toDatedEvent e |> onEvent
      with ex -> eprintfn "%A" ex
      Task.CompletedTask), filterOptions=SubscriptionFilterOptions(filter=EventTypeFilter.ExcludeSystemEvents())).Result
    |> ignore
