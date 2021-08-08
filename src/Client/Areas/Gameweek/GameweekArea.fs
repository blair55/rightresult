namespace Areas.Gameweek

open Elmish
open Shared
open Routes
open Fable.React

module GameweekArea =

  type Model =
    | InitModel
    | GameweekFixturesModel of GameweekFixtures.Model

  type Msg =
    | InitMsg of Result<string, exn>
    | GetEarliestOpenGwno of Rresult<GameweekNo>
    | GameweekFixturesMsg of GameweekFixtures.Msg
    // | NavTo of Route

  let init api player : Model * Cmd<Msg> =
    InitModel,
      Cmd.OfAsync.either
          api.getEarliestOpenGwno
          player.Token
          GetEarliestOpenGwno
          (Error >> InitMsg)

  let update api p message model =
    match message, model with
    | InitMsg _, InitModel -> InitModel, []
    | GetEarliestOpenGwno (Ok (GameweekNo gwno)), InitModel ->
        let m, cmd = GameweekFixtures.init api p (GameweekNo gwno)
        GameweekFixturesModel m,
          Cmd.batch
            [ GameweekRoute(GameweekFixturesRoute gwno) |> Routes.pushTo
              Cmd.map GameweekFixturesMsg cmd
            ]
    | GameweekFixturesMsg msg, GameweekFixturesModel m ->
        GameweekFixtures.update api p msg m |> fun (m, cmd) -> GameweekFixturesModel m, Cmd.map GameweekFixturesMsg cmd
    | _ ->
        model, alert (LoginProblem "couldn't get gw")

  let urlUpdate api p = function
    | GameweekInitRoute -> init api p
    | GameweekFixturesRoute gwno ->
        GameweekFixtures.init api p (GameweekNo gwno) |> fun (m, cmd) -> GameweekFixturesModel m, Cmd.map GameweekFixturesMsg cmd

  let view model dispatch =
    match model with
    | InitModel -> div [] []
    | GameweekFixturesModel m ->
        GameweekFixtures.view m (GameweekFixturesMsg >> dispatch)