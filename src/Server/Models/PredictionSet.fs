namespace Server

open Shared
open Server.Elevated
open Server.Commands
open Server.Events

module PredictionSet =

  type FixtureSetState =
    Map<FixtureId, FixtureRecord>

  type PredictionSetState =
    { Predictions : Map<FixtureId, ScoreLine>
      DoubleDown : FixtureId option
      BigUp : FixtureId option }


  let private fold ({ PredictionSetState.Predictions = predictions } as state) = function
    | PredictionDoubleDownApplied (_, _, fId) ->
      Ok { state with DoubleDown = Some fId }
    | PredictionDoubleDownRemoved _ ->
      Ok { state with DoubleDown = None }
    | PredictionBigUpApplied (_, _, fId) ->
      Ok { state with BigUp = Some fId }
    | PredictionCreated (_, _, fId, sl)
    | PredictionScoreLineSet (_, _, fId, sl) ->
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionHomeScoreSet (_, _, fId, s) ->
      let sl = predictions.[fId] |> fun (ScoreLine (_, a)) -> ScoreLine (s, a)
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionAwayScoreSet (_, _, fId, s) ->
      let sl = predictions.[fId] |> fun (ScoreLine (h, _)) -> ScoreLine (h, s)
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionHomeIncd (_, _, fId) ->
      let sl = predictions.[fId] |> fun (ScoreLine (Score h, a)) -> ScoreLine (Score (h+1), a)
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionAwayIncd (_, _, fId) ->
      let sl = predictions.[fId] |> fun (ScoreLine (h, Score a)) -> ScoreLine (h, Score (a+1))
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionHomeDecd (_, _, fId) ->
      let sl = predictions.[fId] |> fun (ScoreLine (Score h, a)) -> ScoreLine (Score (h-1), a)
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | PredictionAwayDecd (_, _, fId) ->
      let sl = predictions.[fId] |> fun (ScoreLine (h, Score a)) -> ScoreLine (h, Score (a-1))
      Ok { state with Predictions = predictions.Add (fId, sl) }
    | event -> HandlerHelper.eventErr event predictions

  let folder =
    fold, { Predictions = Map.empty
            DoubleDown = None
            BigUp = None }

  let apply (playerId, fsId, cmd, (fixtures:FixtureSetState)) (state:PredictionSetState) : Rresult<Event list> =

    let makeSureFixtureIsEditable (PredictionEditDate date) errMsg { FixtureRecord.KickOff = ko } =
      if Ko.hasKickedOff date ko
      then ValidationError errMsg |> Error
      else Ok ()

    let makeSureFixtureIsBigUpable (PredictionEditDate date) { FixtureRecord.KickOff = ko } =
      if Ko.isLessThanOneHourBeforeKickOff date ko
      then ValidationError "It's too late to Big Up this fixture" |> Error
      else Ok ()

    let datedPredictionCommandHandler fId = function
      | SetScoreLine sl -> [ PredictionScoreLineSet (playerId, fsId, fId, sl) ]
      | SetHomeScore s -> [ PredictionHomeScoreSet (playerId, fsId, fId, s) ]
      | SetAwayScore s -> [ PredictionAwayScoreSet (playerId, fsId, fId, s) ]
      | DoubleDown     -> [ PredictionDoubleDownApplied (playerId, fsId, fId) ]
      | BigUp          -> [ PredictionBigUpApplied (playerId, fsId, fId) ]
      | SimplePredictionCommand spcmd ->
        match spcmd with
        | IncHomeScore -> [ PredictionHomeIncd (playerId, fsId, fId) ]
        | IncAwayScore -> [ PredictionAwayIncd (playerId, fsId, fId) ]
        | DecHomeScore -> [ PredictionHomeDecd (playerId, fsId, fId) ]
        | DecAwayScore -> [ PredictionAwayDecd (playerId, fsId, fId) ]

    let fixtureCreatedEvent fId =
      if state.Predictions.ContainsKey fId then []
      else [ PredictionCreated (playerId, fsId, fId, ScoreLine.Init) ]

    match state.DoubleDown, state.BigUp, cmd with

    | Some ddfId, _, RemoveDoubleDown date ->
      fixtures.[ddfId]
      |> (makeSureFixtureIsEditable date "Double down fixture already kicked off"
      >> Result.map (fun () -> [ PredictionDoubleDownRemoved (playerId, fsId, ddfId) ]))

    | _, Some bufId, DatedPredictionCommand (DoubleDown, fId, _) when fId = bufId ->
      ValidationError "Cannot double down on big up prediction" |> Error

    | Some ddfId, _, DatedPredictionCommand (DoubleDown, fId, date) ->
      fixtures.[ddfId]
      |> (makeSureFixtureIsEditable date "Double down fixture already kicked off"
      >> Result.bind (fun () -> fixtures.[fId] |> makeSureFixtureIsEditable date "Fixture already kicked off")
      >> Result.map (fun () ->
        fixtureCreatedEvent fId @
        [ PredictionDoubleDownRemoved (playerId, fsId, ddfId)
          PredictionDoubleDownApplied (playerId, fsId, fId) ]))

    | Some ddfId, _, DatedPredictionCommand (BigUp, fId, _) when fId = ddfId ->
      ValidationError "Cannot big up on double down prediction" |> Error

    | _, Some _, DatedPredictionCommand (BigUp, _, _) ->
      ValidationError "Cannot big up another prediction" |> Error

    | _, None, DatedPredictionCommand (BigUp, fId, date) ->
      // fixtures.[ddfId]
      // |> (makeSureFixtureIsBigUpable date
      // >> Result.bind (fun () -> fixtures.[fId] |> makeSureFixtureIsEditable date "Fixture already kicked off")
      fixtures.[fId] |> makeSureFixtureIsBigUpable date
      |> Result.map (fun () ->
        fixtureCreatedEvent fId @
        [ PredictionBigUpApplied (playerId, fsId, fId) ])

    | _, Some bufId, DatedPredictionCommand (_, fId, _) when fId = bufId ->
      ValidationError "Cannot edit big up prediction" |> Error

    | _, _, DatedPredictionCommand (pcmd, fId, date) ->
      fixtures.[fId]
      |> (makeSureFixtureIsEditable date "Fixture already kicked off"
      >> Result.map (fun () -> fixtureCreatedEvent fId @ datedPredictionCommandHandler fId pcmd))

    | _, _, OverwritePredictionSet sourcePlayerId ->
      Ok [ PredictionSetOverwritten (sourcePlayerId, playerId, fsId) ]

    | _ -> HandlerHelper.cmdErr cmd state.Predictions
