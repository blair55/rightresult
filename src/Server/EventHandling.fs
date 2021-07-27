namespace Server

open FSharp.Core
open Shared
open Server.Events
open Server.Subscribers

module EventHandling =

  let onEvent deps (DatedEvent (event, created)) =
    printfn "handling event: %A" event

    match event with
    // players
    | PlayerLoggedIn playerId ->
      PlayerLoggedInSubscribers.all
      |> List.iter (fun f -> f deps created playerId)
    | PlayerCreated (playerId, playerName, email) ->
      PlayerCreatedSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, playerName, email))
    | PlayerRemoved playerId ->
      PlayerRemovedSubscribers.all
      |> List.iter (fun f -> f deps created playerId)
    | PlayerSubscribedToPush (playerId, sub) ->
      PlayerSubscribedToPushSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, sub))

    // leagues
    | LeagueCreated (leagueId, leagueName, playerId) ->
      CreateLeagueSubscribers.all
      |> List.iter (fun f -> f deps created (leagueId, leagueName, playerId))
    | LeagueRenamed (leagueId, leagueName) ->
      LeagueRenamedSubscribers.all
      |> List.iter (fun f -> f deps (leagueId, leagueName))
    | LeagueJoined (leagueId, playerId) ->
      LeagueJoinedSubscribers.all
      |> List.iter (fun f -> f deps created (leagueId, playerId))
    | LeagueLeft (leagueId, playerId) ->
      LeagueLeftSubscribers.all
      |> List.iter (fun f -> f deps created (leagueId, playerId))
    | LeagueRemoved leagueId ->
      LeagueRemovedSubscribers.all
      |> List.iter (fun f -> f deps created leagueId)

    // fixtures
    | FixtureSetCreated (fsId, gwno, fixtures) ->
      FixtureSetCreatedSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, gwno, fixtures))

    | FixtureSetConcluded (fsId, gwno) ->
      FixtureSetConcludedSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, gwno))

    | FixtureKoEdited (fsId, fId, ko) ->
      FixtureKoEditedSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, fId, ko))

    | FixtureKickedOff (fsId, fId) ->
      // deps.Queries.getFixtureRecord fId
      // |> fun fixture ->
        // match fixture.HasKickedOff with
        // | true -> printfn "fixture already kicked off: %A" event
        // | _ -> FixtureKickedOffSubscribers.all |> List.iter (fun f -> f deps created (fsId, fId))
      FixtureKickedOffSubscribers.all |> List.iter (fun f -> f deps created (fsId, fId))

    | FixtureClassified (fsId, fId, scoreLine) ->
      // deps.Queries.getFixtureRecord fId
      // |> fun fixture ->
      //   match fixture.ScoreLine with
      //   | Some _ -> printfn "fixture already classified: %A" event
      //   | _ -> FixtureClassifiedSubscribers.all |> List.iter (fun f -> f deps created (fsId, fId, scoreLine))
      FixtureClassifiedSubscribers.all |> List.iter (fun f -> f deps created (fsId, fId, scoreLine))

    | FixtureAppended (fsId, fixture) ->
      FixtureAppendedSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, fixture))

    | FixtureRemoved (fId) ->
      FixtureRemovedSubscribers.all
      |> List.iter (fun f -> f deps created fId)

    // predictions
    | PredictionSetOverwritten (sourcePlayerId, destinationPlayerId, fsId) ->
      PredictionSetOverwrittenSubscribers.all
      |> List.iter (fun f -> f deps created (sourcePlayerId, destinationPlayerId, fsId))

    | PredictionCreated (playerId, fsId, fId, sl) ->
      PredictionCreatedSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId, sl))

    | PredictionHomeIncd (playerId, fsId, fId) ->
      PredictionIncHomeScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionHomeDecd (playerId, fsId, fId) ->
      PredictionDecHomeScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionAwayIncd (playerId, fsId, fId) ->
      PredictionIncAwayScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionAwayDecd (playerId, fsId, fId) ->
      PredictionDecAwayScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionDoubleDownApplied (playerId, fsId, fId) ->
      PredictionDoubleDownAppliedSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionSetDoubleDownRemoved (playerId, fsId) ->
      PredictionSetDoubleDownRemovedSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId))

    | PredictionHomeScoreSet (playerId, fsId, fId, score) -> ()

    | PredictionAwayScoreSet (playerId, fsId, fId, score) -> ()