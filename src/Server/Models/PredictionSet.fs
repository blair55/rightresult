namespace Server

open Shared
open Server.Elevated
open Server.Commands
open Server.Events

module PredictionSet =

  type FixtureSetState =
    Map<FixtureId, FixtureRecord>

  type PredictionSetState =
    Map<FixtureId, ScoreLine> * FixtureId option

  let private fold ((predictions, dd):PredictionSetState) event =
    match dd, event with
    | _, PredictionDoubleDownApplied (_, _, fId) ->
      (predictions, Some fId) |> Ok
    | _, PredictionSetDoubleDownRemoved _ ->
      (predictions, None) |> Ok
    | dd, PredictionCreated (_, _, fId, sl) ->
      (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionHomeScoreSet (_, _, fId, s) ->
      predictions.[fId]
      |> fun (ScoreLine (_, a)) -> ScoreLine (s, a)
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionAwayScoreSet (_, _, fId, s) ->
      predictions.[fId]
      |> fun (ScoreLine (h, _)) -> ScoreLine (h, s)
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionHomeIncd (_, _, fId) ->
      predictions.[fId]
      |> fun (ScoreLine (Score h, a)) -> ScoreLine (Score (h+1), a)
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionAwayIncd (_, _, fId) ->
      predictions.[fId]
      |> fun (ScoreLine (h, Score a)) -> ScoreLine (h, Score (a+1))
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionHomeDecd (_, _, fId) ->
      predictions.[fId]
      |> fun (ScoreLine (Score h, a)) -> ScoreLine (Score (h-1), a)
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | dd, PredictionAwayDecd (_, _, fId) ->
      predictions.[fId]
      |> fun (ScoreLine (h, Score a)) -> ScoreLine (h, Score (a-1))
      |> fun sl -> (predictions.Add (fId, sl), dd) |> Ok
    | _ -> HandlerHelper.eventErr event predictions

  let folder =
    fold, (Map.empty, None)

  let apply (playerId, fsId, cmd, (fixtures:FixtureSetState)) ((predictions, dd):PredictionSetState) : Rresult<Event list> =

    let makeSureFixtureIsEditable (PredictionEditDate date) errMsg { FixtureRecord.KickOff = (KickOff ko) } =
      if date < ko then Ok ()
      else ValidationError errMsg |> Error

    let datedPredictionCommandHandler fId = function
      | SetHomeScore s -> [ PredictionHomeScoreSet (playerId, fsId, fId, s) ]
      | SetAwayScore s -> [ PredictionAwayScoreSet (playerId, fsId, fId, s) ]
      | DoubleDown     -> [ PredictionDoubleDownApplied (playerId, fsId, fId) ]
      | SimplePredictionCommand spcmd ->
        match spcmd with
        | IncHomeScore -> [ PredictionHomeIncd (playerId, fsId, fId) ]
        | IncAwayScore -> [ PredictionAwayIncd (playerId, fsId, fId) ]
        | DecHomeScore -> [ PredictionHomeDecd (playerId, fsId, fId) ]
        | DecAwayScore -> [ PredictionAwayDecd (playerId, fsId, fId) ]

    let fixtureCreatedEvent fId =
      if predictions.ContainsKey fId then []
      else [ PredictionCreated (playerId, fsId, fId, ScoreLine.Init) ]

    match dd, cmd with
    | Some ddfId, RemoveDoubleDown date ->
      fixtures.[ddfId]
      |> (makeSureFixtureIsEditable date "Fixture already kicked off"
      >> Result.map (fun () -> [ PredictionSetDoubleDownRemoved (playerId, fsId) ]))

    | Some ddfId, DatedPredictionCommand (DoubleDown, fId, date) ->
      fixtures.[ddfId]
      |> (makeSureFixtureIsEditable date "Double down fixture already kicked off"
      >> Result.bind (fun () -> fixtures.[fId] |> makeSureFixtureIsEditable date "Fixture already kicked off")
      >> Result.map (fun () ->
        fixtureCreatedEvent fId @
        [ PredictionSetDoubleDownRemoved (playerId, fsId)
          PredictionDoubleDownApplied (playerId, fsId, fId) ]))

    | _, DatedPredictionCommand (pcmd, fId, date) ->
      fixtures.[fId]
      |> (makeSureFixtureIsEditable date "Fixture already kicked off"
      >> Result.map (fun () -> fixtureCreatedEvent fId @ datedPredictionCommandHandler fId pcmd))

    | _, OverwritePredictionSet sourcePlayerId ->
      Ok [ PredictionSetOverwritten (sourcePlayerId, playerId, fsId) ]

    | _ -> HandlerHelper.cmdErr cmd predictions
