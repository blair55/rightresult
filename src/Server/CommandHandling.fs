namespace Server

open Shared
open Server.Elevated
open Server.Events
open Server.Commands

module CommandHandler =

  let foldToResult (fold, init) =
    List.fold
      (fun r e -> r |> Result.bind (fun (v, s) -> fold s e |> Result.bind (fun s -> Result.retn (incVer v, s))))
      (Result.retn (initEventVersion, init))

  let cleanEvents =
    List.map (fun (DatedEvent (e, _)) -> e)

  let handle (readEvents:ReadEvents) (storeEvents:StoreEvents) (command:Command) : Ars<Unit> =

    let getState folder =
      readEvents >> AsyncResult.map cleanEvents >> Async.map (Result.bind (foldToResult folder))

    let applyAndStore apply streamId (v, state) =
      state |> (apply >> Async.retn >> AsyncResult.bind (storeEvents streamId v))

    let getStateApplyAndStore streamId folder apply =
      streamId |> (getState folder >> AsyncResult.bind (applyAndStore apply streamId))

    match command with

    | PredictionSetCommand (PlayerId playerId, FixtureSetId fsId, cmd) ->
      StreamId (sprintf "fixtureset-%A" fsId)
      |> getState FixtureSet.folder
      |> AsyncResult.map (fun (_, fs) -> fs)
      |> AsyncResult.bind (fun fixtureSet ->
         getStateApplyAndStore
          (StreamId (sprintf "predictionset-%s-%s" playerId (string fsId)))
          PredictionSet.folder
          (PredictionSet.apply (PlayerId playerId, FixtureSetId fsId, cmd, fixtureSet)))

    | FixtureSetCommand (FixtureSetId fsId, cmd) ->
      getStateApplyAndStore
        (StreamId (sprintf "fixtureset-%s" (string fsId)))
        FixtureSet.folder
        (FixtureSet.apply (FixtureSetId fsId, cmd))

    | PlayerCommand (PlayerId playerId, cmd) ->
      getStateApplyAndStore
        (StreamId (sprintf "player-%s" playerId))
        Player.folder
        (Player.apply (PlayerId playerId, cmd))

    | PrivateLeagueCommand (PrivateLeagueId leagueId, cmd) ->
      getStateApplyAndStore
        (StreamId (sprintf "league-%s" (string leagueId)))
        League.folder
        (League.apply (PrivateLeagueId leagueId, cmd))
