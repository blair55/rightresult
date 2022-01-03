namespace Server

open Shared
open Server.Commands
open Server.Events

module FixtureSet =

  type FixtureSetState =
    Map<FixtureId, FixtureRecord>

  let private fold (fixtures:FixtureSetState) event =
    match event with

    | FixtureSetCreated (_, _, fixtures) ->
      fixtures
      |> List.map (fun f -> f.Id, f) |> Map.ofList |> Ok

    | FixtureSetConcluded (_, _) ->
      Ok fixtures

    | FixtureKoEdited (_, fId, ko) ->
      fixtures.[fId]
      |> fun f -> fixtures.Add (fId, { f with KickOff = ko }) |> Ok

    | FixtureKickedOff (_, fId) ->
      fixtures
      |> Map.change fId (Option.map (fun f -> { f with State = FixtureState.InPlay (ScoreLine.Init, MinutesPlayed.init) })) |> Ok

    | FixtureReclassified (_, fId, sl)
    | FixtureClassified (_, fId, sl) ->
      fixtures
      |> Map.change fId (Option.map (fun f -> { f with State = FixtureState.Classified sl })) |> Ok

    | FixtureAppended (_, f) ->
      fixtures.Add (f.Id, f) |> Ok

    | FixtureRemoved fId ->
      fixtures.Remove fId |> Ok

    | _ -> HandlerHelper.eventErr event fixtures

  let folder =
    fold, Map.empty


  let makeSureFixtureIsOpen { FixtureRecord.State = state } =
    match state with
    | FixtureState.Open _ -> Ok ()
    | _ -> ValidationError "Fixture not open" |> Error

  let makeSureFixtureIsInPlay { FixtureRecord.State = state; Id = fId; GameweekNo = gwno } =
    match state with
    | FixtureState.InPlay _ -> Ok ()
    | _ -> ValidationError $"Fixture not in play {gwno} {fId}" |> Error

  let makeSureFixtureIsClassified { FixtureRecord.State = state } =
    match state with
    | FixtureState.Classified sl -> Ok sl
    | _ -> ValidationError "Fixture not classified" |> Error

  let apply (fsId, cmd) (fixtures:FixtureSetState) : Rresult<Event list> =
    match cmd with

    | CreateFixtureSet (gwno, fixtures) ->
      [ FixtureSetCreated (fsId, gwno, fixtures) ] |> Ok

    | ConcludeFixtureSet gwno ->
      [ FixtureSetConcluded (fsId, gwno) ] |> Ok

    | EditFixtureKickOff (fId, ko) ->
      fixtures.[fId] |> makeSureFixtureIsOpen |> Result.map (fun () -> [ FixtureKoEdited (fsId, fId, ko) ])

    | KickOffFixture fId ->
      fixtures.[fId] |> makeSureFixtureIsOpen |> Result.map (fun () -> [ FixtureKickedOff (fsId, fId) ])

    | ClassifyFixture (fId, sl) ->
      fixtures.[fId] |> makeSureFixtureIsInPlay |> Result.map (fun () -> [ FixtureClassified (fsId, fId, sl) ])

    | ReclassifyFixture (fId) ->
      fixtures.[fId] |> makeSureFixtureIsClassified |> Result.map (fun sl -> [ FixtureReclassified (fsId, fId, sl) ])

    | AppendFixture fixture ->
      [ FixtureAppended (fsId, fixture) ] |> Ok

    | RemoveOpenFixture fId ->
      Ok [ FixtureRemoved fId ]
