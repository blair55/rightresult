namespace Areas.Gameweek

open Elmish
open Shared
open Routes
open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Fulma
open Areas
open Components

module GameweekFixtures =

  type ModalState =
    | ModalOpen of FixturePredictionViewModel
    | ModalClosed

  type Model =
    { GameweekFixtures: GameweekFixturesViewModel WebData
      ModalState: ModalState }

  type Msg =
    | Init of Result<string, exn>
    | GameweekFixturesReceived of Rresult<GameweekFixturesViewModel>
    | ShowModal of FixturePredictionViewModel
    | HideModal
    | NavTo of Route

  let init api p gwno =
    { GameweekFixtures = Fetching
      ModalState = ModalClosed },
    Cmd.OfAsync.either (api.getGameweekFixtures p.Token) gwno GameweekFixturesReceived (Error >> Init)

  let pageFixtureButton dispatch text fixtures =
    Option.bind (fun fId -> Map.tryFind fId fixtures)
    >> function
      | Some (fp: FixturePredictionViewModel) ->
        Button.button [ Button.Color IsLight
                        Button.Props [ OnClick(fun _ -> dispatch (ShowModal fp)) ] ] [
          str text
        ]
      | None ->
        Button.button [ Button.Disabled true ] [
          str text
        ]

  let rowOf3 left center right =
    div [ Style [ PaddingBottom "0.5em" ] ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Right) ]
            Column.Width(Screen.All, Column.IsTwoFifths) ]
          left
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
            Column.Width(Screen.All, Column.IsOneFifth) ]
          center
        Column.column
          [ Column.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Left) ]
            Column.Width(Screen.All, Column.IsTwoFifths) ]
          right
      ]
    ]

  let homeTeam home =
    Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.RowReverse
                           Modifier.FlexWrap FlexWrap.NoWrap
                           Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
      div [ Style [ PaddingLeft "0.3em" ] ] [
        Components.badge Components.BadgeSize.M home
      ]
      Components.shortTeamName home
    ]

  let awayTeam away =
    Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Row
                           Modifier.FlexWrap FlexWrap.NoWrap
                           Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
      div [ Style [ PaddingRight "0.3em" ] ] [
        Components.badge Components.BadgeSize.M away
      ]
      Components.shortTeamName away
    ]

  let openFixture
    dispatch
    ({ TeamLine = TeamLine (home, away)
       Prediction = pred } as fp: FixturePredictionViewModel)
    =
    let prediction =
      match pred with
      | Some sl -> ScoreBox.openScoreBox sl
      | None -> ScoreBox.emptyScoreBox ()

    let editButton =
      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ] ] [
        Button.button
          ([ Button.Size IsSmall
             Button.Color IsWarning
             Button.OnClick(fun _ -> dispatch (ShowModal fp)) ])
          [ div [] [
              span [ Style [ MarginLeft "2px" ] ] [
                str "Predict"
              ]
              Fa.i [ Fa.Solid.AngleRight
                     Fa.Size Fa.FaSmall ] []
            ] ]
      ]

    div [] [
      rowOf3 [ homeTeam home ] [ Components.kickOffTime fp.KickOff ] [ awayTeam away ]
      rowOf3 [] [ prediction ] [ editButton ]
    ]

  let kickedOffFixture
    dispatch
    ({ TeamLine = TeamLine (home, away)
       Prediction = pred } as fp: FixturePredictionViewModel)
    =
    let prediction =
      match pred with
      | Some sl ->
        Text.div [ Modifiers [ Modifier.FlexWrap FlexWrap.NoWrap
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ] ] [
          ScoreBox.kickedOffScoreBox sl fp.IsDoubleDown
        ]
      | None ->
        Text.div [ Modifiers [ Modifier.FlexWrap FlexWrap.NoWrap
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ] ] [
          ScoreBox.emptyScoreBox ()
        ]

    let reviewButton =
      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ] ] [
      Button.button
        ([ Button.Size IsSmall
           Button.Color IsWarning
           Button.OnClick(fun _ -> dispatch (ShowModal fp)) ])
        [ div [] [
            span [ Style [ MarginLeft "2px" ] ] [
              str "view"
            ]
            Fa.i [ Fa.Solid.AngleRight
                   Fa.Size Fa.FaSmall ] []
          ] ]
      ]

    div [] [
      rowOf3 [ homeTeam home ] [ ScoreBox.openScoreBox (ScoreLine.Init) ] [ awayTeam away ]
      rowOf3 [] [ prediction ] [ reviewButton ]
    ]

  let classifiedFixture
    dispatch
    ({ TeamLine = TeamLine (home, away)
       Prediction = pred } as fp: FixturePredictionViewModel)
    (sl, points, rc)
    =
    let prediction =
      match pred with
      | Some sl ->
        Text.div [ Modifiers [ Modifier.FlexWrap FlexWrap.NoWrap
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ] ] [
          ScoreBox.kickedOffScoreBox sl fp.IsDoubleDown
        ]
      | None ->
        Text.div [ Modifiers [ Modifier.FlexWrap FlexWrap.NoWrap
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ] ] [
          ScoreBox.emptyScoreBox ()
        ]

    let reviewButton =
      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ] ] [
      Button.button
        ([ Button.Size IsSmall
           Button.Color IsWarning
           Button.OnClick(fun _ -> dispatch (ShowModal fp)) ])
        [ div [] [
            span [ Style [ MarginLeft "2px" ] ] [
              str "points"
            ]
            Fa.i [ Fa.Solid.AngleRight
                   Fa.Size Fa.FaSmall ] []
          ] ]
      ]

    div [] [
      rowOf3 [ homeTeam home ] [ ScoreBox.resultScoreBox sl ] [ awayTeam away ]
      rowOf3 [] [ prediction ] [ reviewButton ]
    ]

  let fixtureViewFlex dispatch (fp: FixturePredictionViewModel) =
    match fp.State with
    | FixtureState.Open -> openFixture dispatch fp
    | FixtureState.KickedOff -> kickedOffFixture dispatch fp
    | FixtureState.Classified (sl, points, rc) -> classifiedFixture dispatch fp (sl, points, rc)

  let fixtureGroup dispatch (koStr, fixtures) =
    div [ Style [ MarginBottom "1.5em" ] ] [ // Heading.h5 [ Heading.IsSubtitle ] [ str koStr ]
      div [ Style [ MarginBottom "1em" ] ] [
        Components.gameweekDate koStr
      ]
      div
        [ Class "gw-fixture-list" ]
        (fixtures
         |> List.map
              (fun f ->
                div [ Class "gw-fixture-item" ] [
                  fixtureViewFlex dispatch f
                ]))
    ]

  let pageGwButton dispatch text =
    function
    | Some (GameweekNo gwno) ->
      Button.button [ Button.Color IsLight
                      Button.Props [ OnClick(fun _ -> dispatch (NavTo(GameweekRoute(GameweekFixturesRoute gwno)))) ] ] [
        str text
      ]
    | None ->
      Button.button [ Button.Disabled true ] [
        str text
      ]

  let fixtureModal dispatch (GameweekNo gwno) (fixtures) (modalState) =
    match modalState with
    | ModalOpen ({ TeamLine = TeamLine (ht, at)
                   Neighbours = (prev, next) } as fp: FixturePredictionViewModel) ->

      Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick (fun _ -> dispatch HideModal) ] [ ]
        Modal.Card.card [] [
          Modal.Card.head [] [
            Modal.Card.title [] [
              str (sprintf "Gameweek %i" gwno)
            ]
            Delete.delete [ Delete.OnClick(fun _ -> dispatch HideModal) ] []
          ]
          Modal.Card.body [] [
            teamName ht
            teamName at
          ]
          Modal.Card.foot [] [
            pageFixtureButton dispatch "<<" fixtures prev
            pageFixtureButton dispatch ">>" fixtures next

          ]
        ]

      ]

    | ModalClosed -> div [] []

  let fullView
    dispatch
    ({ GameweekNo = GameweekNo gwno
       Neighbours = prev, next } as gwfs: GameweekFixturesViewModel)
    modalState
    =
    div [] [
      Components.pageTitle (sprintf "Gameweek %i" gwno)
      div [ Style [ MarginBottom "1em" ] ] [
        pageGwButton dispatch "<<" prev
        pageGwButton dispatch ">>" next
      ]
      div
        []
        (Map.toList gwfs.Fixtures
         |> List.map snd
         |> List.sortBy (fun f -> f.SortOrder)
         |> List.groupBy (fun f -> f.KickOffString)
         |> List.map (fixtureGroup dispatch))
      fixtureModal dispatch gwfs.GameweekNo gwfs.Fixtures modalState
    ]

  let view (model: Model) dispatch =
    match model.GameweekFixtures with
    | Success gwfs -> fullView dispatch gwfs model.ModalState
    | WebError e -> div [] [ str "error" ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | GameweekFixturesReceived r ->
      { model with
          GameweekFixtures = resultToWebData r },
      []
    | ShowModal fp -> { model with ModalState = ModalOpen fp }, []
    | HideModal -> { model with ModalState = ModalClosed }, []
    | NavTo r -> model, (Routes.navTo r)
