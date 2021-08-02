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

    | FixtureKickedOff _ ->
      Ok fixtures

    | FixtureClassified (_, fId, sl) ->
      fixtures.[fId]
      |> fun f -> fixtures.Add (fId, { f with State = FixtureState.Classified sl }) |> Ok

    | FixtureAppended (_, f) ->
      fixtures.Add (f.Id, f) |> Ok

    | FixtureRemoved fId ->
      fixtures.Remove fId |> Ok

    | _ -> HandlerHelper.eventErr event fixtures

  let folder =
    fold, Map.empty

  let makeSureFixtureIsOpen { FixtureRecord.State = state } =
    match state with
    | FixtureState.Open -> Ok ()
    | _ -> ValidationError "Fixture not open" |> Error

  let apply (fsId, cmd) (fixtures:FixtureSetState) : Rresult<Event list> =
    match cmd with

    | CreateFixtureSet (gwno, fixtures) ->
      [ FixtureSetCreated (fsId, gwno, fixtures) ] |> Ok

    | ConcludeFixtureSet gwno ->
      [ FixtureSetConcluded (fsId, gwno) ] |> Ok

    | EditFixtureKickOff (fId, ko) ->
      fixtures.[fId] |> (makeSureFixtureIsOpen >> Result.map (fun () -> [ FixtureKoEdited (fsId, fId, ko) ]))

    | KickOffFixture fId ->
      [ FixtureKickedOff (fsId, fId) ] |> Ok

    | ClassifyFixture (fId, sl) ->
      [ FixtureClassified (fsId, fId, sl) ] |> Ok

    | AppendFixture fixture ->
      [ FixtureAppended (fsId, fixture) ] |> Ok

    | RemoveOpenFixture fId ->
      fixtures.[fId] |> makeSureFixtureIsOpen |> Result.map (fun () -> [ FixtureRemoved fId ])
