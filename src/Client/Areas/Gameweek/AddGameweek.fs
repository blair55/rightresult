namespace Areas.Gameweek

open Elmish

open Fable.React
open Fable.React.Props

open System
open Shared
open Fulma
open Routes
open Areas

module AddGameweek =

  type Model =
    { FixtureSet: NewFixtureSetViewModel WebData
      IsSubmitting: bool }

  type Msg =
    | Init of Result<string, exn>
    | GetNewFixtureSetResult of Rresult<NewFixtureSetViewModel>
    | SubmitFixtureSet
    | SubmitFixtureSetResult of Rresult<unit>
    | NavTo of Route

  let init api player : Model * Cmd<Msg> =
    { FixtureSet = Fetching
      IsSubmitting = false },
    Cmd.OfAsync.either api.getNewFixtureSet player.Token GetNewFixtureSetResult (Error >> Init)

  let button attr txt onClick =
    Button.button
      ([ Button.IsFullWidth
         Button.Color IsPrimary
         Button.OnClick onClick ]
       @ attr)
      [ str txt ]

  let addFixtureSetButton model dispatch =
    match model.IsSubmitting with
    | false -> button [] "Add Fixtures" (fun _ -> dispatch SubmitFixtureSet)
    | true -> button [ Button.IsLoading true ] "" ignore

  let rowOf3 one two three =
    div [ Style [ PaddingBottom "0.5em" ] ] [
      Columns.columns [ Columns.IsMobile; Columns.IsGapless ] [
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Right) ]
            Column.Width(Screen.All, Column.IsTwoFifths) ] one
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
            Column.Width(Screen.All, Column.IsOneFifth) ] two
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Left) ]
            Column.Width(Screen.All, Column.IsTwoFifths) ] three
      ]
    ]

  let fixtureViewFlex (kickoff, _, TeamLine (home, away)) =
    rowOf3
      [ Text.div
          [ Modifiers
              [ Modifier.FlexDirection FlexDirection.RowReverse
                Modifier.FlexWrap FlexWrap.NoWrap
                Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly
                // Modifier.FlexAlignItems FlexAlignItems.Stretch
                // Modifier.FlexAlignContent FlexAlignContent.Stretch
              ]
          ]
          [
            Components.badge Components.BadgeSize.M home
            Components.shortTeamName home
          ]
      ]
      [ Components.kickOffTime kickoff ]
      [ Text.div
          [ Modifiers
              [ Modifier.FlexDirection FlexDirection.Row
                Modifier.FlexWrap FlexWrap.NoWrap
                Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly
              ]
          ]
          [ Components.badge Components.BadgeSize.M away
            Components.shortTeamName away
          ]
      ]

  let fixtureGroup (koStr, fixtures) =

    div [ Style [ MarginBottom "1.5em" ] ] [ // Heading.h5 [ Heading.IsSubtitle ] [ str koStr ]
      div [ Style [ MarginBottom "1em" ] ] [
        Components.gameweekDate koStr
      ]
      div [] (fixtures |> List.map fixtureViewFlex)
    ]

  let fixturesView =
    function
    | Success (f: NewFixtureSetViewModel) ->
      f.Fixtures
      |> List.groupBy (fun (_, koStr, _) -> koStr)
      |> List.map fixtureGroup
      |> div []
    | _ -> div [] []

  let gwNo =
    function
    | Success { NewFixtureSetViewModel.GameweekNo = (GameweekNo gwno) } -> gwno
    | _ -> 0

  let view (model: Model) dispatch =
    div [] [
      Components.pageTitle (sprintf "Add GW %i" (gwNo model.FixtureSet))
      fixturesView model.FixtureSet
      Card.card [] [
        Card.footer [] [
          Card.Footer.a [] [
            addFixtureSetButton model dispatch
          ]
        ]
      ]
    ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | GetNewFixtureSetResult r ->
      { model with
          FixtureSet = resultToWebData r },
      []
    | SubmitFixtureSet ->
      { model with IsSubmitting = true },
      Cmd.OfAsync.either api.addNewFixtureSet player.Token SubmitFixtureSetResult (Error >> Init)
    | SubmitFixtureSetResult r ->
      match r with
      | Ok () -> model, Cmd.OfPromise.either delay (GameweekRoute (GameweekInitRoute)) NavTo (Error >> Init)
      | Error e -> model, alert e
    | NavTo r -> model, (Routes.navTo r)
