namespace Areas.Fixtures

open Elmish
open Shared
open Routes

module FixturesArea =

  type Model =
    | OmniFixturesModel of OmniFixtures.Model
    | AddFixtureSetModel of AddFixtureSet.Model

  type Msg =
    | OmniFixturesMsg of OmniFixtures.Msg
    | AddFixtureSetMsg of AddFixtureSet.Msg

  let update api p message model =
    match message, model with
    | OmniFixturesMsg msg, OmniFixturesModel m ->
      OmniFixtures.update api p msg m |> fun (m, cmd) -> OmniFixturesModel m, Cmd.map OmniFixturesMsg cmd
    | AddFixtureSetMsg msg, AddFixtureSetModel m ->
      AddFixtureSet.update api p msg m |> fun (m, cmd) -> AddFixtureSetModel m, Cmd.map AddFixtureSetMsg cmd
    | _ -> model, alert (LoginProblem "fixture msg not matched")

  let urlUpdate api p = function
    | OmniFixturesRoute ->
      OmniFixtures.init api p |> fun (m, cmd) -> OmniFixturesModel m, Cmd.map OmniFixturesMsg cmd
    | AddFixtureSetRoute ->
      AddFixtureSet.init api p |> fun (m, cmd) -> AddFixtureSetModel m, Cmd.map AddFixtureSetMsg cmd

  let view model dispatch =
    match model with
    | OmniFixturesModel m ->
      OmniFixtures.view m (OmniFixturesMsg >> dispatch)
    | AddFixtureSetModel m ->
      AddFixtureSet.view m (AddFixtureSetMsg >> dispatch)
