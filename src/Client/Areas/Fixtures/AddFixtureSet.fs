namespace Areas.Fixtures

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

open System
open Shared
open Fulma
open Routes
open Areas

module AddFixtureSet =

  type Model =
    { FixtureSet : NewFixtureSetViewModel WebData
      IsSubmitting : bool
    }

  type Msg =
    | Init of Result<string, exn>
    | GetNewFixtureSetResult of Rresult<NewFixtureSetViewModel>
    | SubmitFixtureSet
    | SubmitFixtureSetResult of Rresult<unit>
    | NavTo of Route

  let init api player : Model * Cmd<Msg> =
    { FixtureSet = Fetching
      IsSubmitting = false
    }, Cmd.ofAsync
        api.getNewFixtureSet
        player.Token
        GetNewFixtureSetResult
        (Error >> Init)

  let button attr txt onClick =
    Button.button
      ([ Button.IsFullWidth
         Button.Color IsPrimary
         Button.OnClick onClick ] @ attr)
      [ str txt ]

  let addFixtureSetButton model dispatch =
    match model.IsSubmitting with
    | false -> button [] "Add Fixtures" (fun _ -> dispatch SubmitFixtureSet)
    | true  -> button [Button.IsLoading true] "" ignore

  let rowOf3 one two three =
    div []
      [ Columns.columns [ Columns.IsMobile; Columns.IsGapless]
          [ Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Right) ]; Column.Width (Screen.All, Column.IsTwoFifths) ] one
            Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]; Column.Width (Screen.All, Column.IsOneFifth) ] two
            Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ]; Column.Width (Screen.All, Column.IsTwoFifths) ] three
          ]
      ]

  let fixtureView (KickOff ko, _, home, away) =
    rowOf3
      [ Components.teamName home ]
      [ Tag.tag [] [ str (ko.DateTime.ToString("HH:mm")) ] ]
      [ Components.teamName away ]

  let fixtureGroup (koStr, fixtures) =
    div [ Style [ MarginBottom "1.5em" ] ]
      [ Heading.h5 [ Heading.IsSubtitle ] [ str koStr ]
        div [] (fixtures |> List.map fixtureView)
      ]

  let fixturesView = function
    | Success f ->
      f.Fixtures
      |> List.groupBy (fun (_, koStr, _, _) -> koStr)
      |> List.map fixtureGroup
      |> div []
    | _ -> div [] []

  let gwNo = function
    | Success { NewFixtureSetViewModel.GameweekNo = (GameweekNo gwno) }-> gwno
    | _ -> 0

  let view (model:Model) dispatch =
    div []
      [ Components.pageTitle "Add Fixtures"
        Card.card []
          [ Card.Header.title
              [ Card.Header.Title.Modifiers
                  [ Modifier.BackgroundColor IsPrimary
                    Modifier.TextTransform TextTransform.UpperCase
                    Modifier.TextSize (Screen.All, TextSize.Is7)
                    Modifier.TextColor IsWhite
                    Modifier.IsShadowless
                  ]
                Card.Header.Title.Props
                  [ Style
                      [ Padding "0.5em 1.4em"
                      ]
                  ]
              ]
              [ Text.span [] [ str <| sprintf "GW %i" (gwNo model.FixtureSet) ]
              ]
            Card.content []
              [ fixturesView model.FixtureSet
              ]
            Card.footer []
              [ Card.Footer.item []
                  [ addFixtureSetButton model dispatch
                  ]
              ]
          ]
      ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | GetNewFixtureSetResult r ->
      { model with FixtureSet = resultToWebData r }, []
    | SubmitFixtureSet ->
      { model with IsSubmitting = true },
      Cmd.ofAsync
        api.addNewFixtureSet
        player.Token
        SubmitFixtureSetResult
        (Error >> Init)
    | SubmitFixtureSetResult r ->
      match r with
      | Ok () -> model, Cmd.ofPromise delay (FixtureRoute OmniFixturesRoute) NavTo (Error >> Init)
      | Error e -> model, alert e
    | NavTo r ->
      model, (Routes.navTo r)
