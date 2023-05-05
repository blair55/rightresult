namespace Server

open FSharp.Core
open Shared
open Server.Events
open Server.Subscribers

module EventHandling =

  let (|MapPredictionSetEventPlayerIdId|) (PlayerId playerId) : PlayerId =
    match playerId with
    | "fb-10160345161488703" -> PlayerId "tw-414832159"
    | "fb-735238821018" -> PlayerId "tw-458540585"
    | "tw-462436725" -> PlayerId "fb-10152951016485684"
    | "tw-414832159" -> PlayerId "fb-10160345161488703"
    | _ -> PlayerId playerId

  let onEvent deps (DatedEvent (event, created)) =
    printfn "handling event: %A" event

    match event with
    // players
    | PlayerLoggedIn (playerId) ->
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
      FixtureKickedOffSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, fId))

    | FixtureClassified (fsId, fId, scoreLine) ->
      deps.Queries.getFixtureRecord fId
      |> fun fixture ->
        match fixture.State with
        | FixtureState.InPlay _ ->
            FixtureClassifiedSubscribers.all
            |> List.iter (fun f -> f deps created (fsId, fId, scoreLine))
        | _ -> printfn "fixture already classified: %A" event

    | FixtureReclassified (fsId, fId, scoreLine) ->
      FixtureClassifiedSubscribers.all
      |> List.iter (fun f -> f deps created (fsId, fId, scoreLine))

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

    | PredictionCreated (MapPredictionSetEventPlayerIdId playerId, fsId, fId, sl) ->
      PredictionCreatedSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId, sl))

    | PredictionHomeIncd (MapPredictionSetEventPlayerIdId playerId, fsId, fId) ->
      PredictionIncHomeScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionHomeDecd (MapPredictionSetEventPlayerIdId playerId, fsId, fId) ->
      PredictionDecHomeScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionAwayIncd (MapPredictionSetEventPlayerIdId playerId, fsId, fId) ->
      PredictionIncAwayScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionAwayDecd (MapPredictionSetEventPlayerIdId playerId, fsId, fId) ->
      PredictionDecAwayScoreSubscribers.all
      |> List.iter (fun f -> f deps created (playerId, fsId, fId))

    | PredictionBigUpApplied (MapPredictionSetEventPlayerIdId playerId, _, fId) ->
      PredictionBigUpAppliedSubscribers.all
      |> List.iter (fun f -> f deps (playerId, fId))

    | PredictionDoubleDownApplied (MapPredictionSetEventPlayerIdId playerId, _, fId) ->
      PredictionDoubleDownAppliedSubscribers.all
      |> List.iter (fun f -> f deps (playerId, fId))

    | PredictionDoubleDownRemoved (MapPredictionSetEventPlayerIdId playerId, _, fId) ->
      PredictionDoubleDownRemovedSubscribers.all
      |> List.iter (fun f -> f deps (playerId, fId))

    | PredictionHomeScoreSet (MapPredictionSetEventPlayerIdId playerId, fsId, fId, score) ->
      PredictionSetHomeScoreSubscribers.all
      |> List.iter (fun f -> f deps playerId fId score)

    | PredictionAwayScoreSet (MapPredictionSetEventPlayerIdId playerId, fsId, fId, score) ->
      PredictionSetAwayScoreSubscribers.all
      |> List.iter (fun f -> f deps playerId fId score)

    | PredictionScoreLineSet (MapPredictionSetEventPlayerIdId playerId, fsId, fId, scoreline) ->
      PredictionSetScoreSubscribers.all
      |> List.iter (fun f -> f deps playerId fId scoreline)
