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
    | ModalOpen of FixtureId
    | ModalClosed

  type Model =
    { GameweekFixtures: GameweekFixturesViewModel WebData
      ModalState: ModalState }

  type Msg =
    | Init of Result<string, exn>
    | GameweekFixturesReceived of Rresult<GameweekFixturesViewModel>
    | ShowModal of FixtureId
    | HideModal
    | NavTo of Route
    | Prediction of PredictionAction
    | PredictionAccepted of Rresult<PredictionAction>
    | SetDoubleDown of FixtureSetId * FixtureId
    | SetDoubleDownResponse of Rresult<FixtureSetId * FixtureId>
    | RemoveDoubleDown of FixtureSetId
    | RemoveDoubleDownResponse of Rresult<FixtureSetId>

  let init api p gwno =
    { GameweekFixtures = Fetching
      ModalState = ModalClosed },
    Cmd.OfAsync.either (api.getGameweekFixtures p.Token) gwno GameweekFixturesReceived (Error >> Init)

  let pageFixtureButton dispatch text fixtures =
    Option.bind (fun fId -> Map.tryFind fId fixtures)
    >> function
      | Some (fp: FixturePredictionViewModel) ->
        Button.button [ Button.Color IsLight
                        Button.Props [ OnClick(fun _ -> dispatch (ShowModal fp.Id)) ] ] [
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
             Button.OnClick(fun _ -> dispatch (ShowModal fp.Id)) ])
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
             Button.OnClick(fun _ -> dispatch (ShowModal fp.Id)) ])
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
    (sl)
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
             Button.OnClick(fun _ -> dispatch (ShowModal fp.Id)) ])
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

  let fixtureSwitch dispatch (fp: FixturePredictionViewModel) open' inplay classified =
    match fp.State with
    | FixtureState.Open -> open' dispatch fp
    | FixtureState.InPlay(sl, mp) -> inplay dispatch fp
    | FixtureState.Classified (sl) -> classified dispatch fp (sl)

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
                  fixtureSwitch dispatch f openFixture kickedOffFixture classifiedFixture
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

  let button atts onClick content =
    Button.button
      ([ Button.IsFullWidth
         Button.OnClick onClick ]
       @ atts)
      content

  let disabledIcon i =
    Fa.i [ i
           Fa.Props [ Style [ Color "#b5b5b5" ] ] ] []

  let scoreIncButton dispatch (f: FixturePredictionViewModel, team) =
    button
      [ Button.Color IsLight ]
      (if f.InProgress then
         ignore
       else
         fun _ ->
           PredictionAction(f.FixtureSetId, f.Id, team, Inc)
           |> Prediction
           |> dispatch)
      [ Fa.i [ Fa.Solid.AngleUp ] [] ]

  let scoreDecButton dispatch (f: FixturePredictionViewModel, team, score: int option) =
    match score with
    | Some s when s > 0 ->
      button
        [ Button.Color IsLight ]
        (if f.InProgress then
           ignore
         else
           fun _ ->
             PredictionAction(f.FixtureSetId, f.Id, team, Dec)
             |> Prediction
             |> dispatch)
        [ Fa.i [ Fa.Solid.AngleDown ] [] ]
    | _ -> disabledIcon Fa.Solid.AngleDown

  let doubleDownButton dispatch isDoubleDownAvailable (f: FixturePredictionViewModel) =
    let icon = [ Fa.i [ Fa.Solid.AngleDoubleDown ] [] ]

    match isDoubleDownAvailable, f.Prediction, f.IsDoubleDown with
    | true, Some _, false ->
      button
        [ Button.Color IsLight ]
        (if f.InProgress then
           ignore
         else
           fun _ -> SetDoubleDown(f.FixtureSetId, f.Id) |> dispatch)
        icon
    | true, Some _, true ->
      button
        [ Button.Color IsWarning ]
        (if f.InProgress then
           ignore
         else
           fun _ -> RemoveDoubleDown f.FixtureSetId |> dispatch)
        icon
    | _ -> disabledIcon Fa.Solid.AngleDoubleDown

  let openFixtureModalContent isDoubleDownAvailable dispatch (fp: FixturePredictionViewModel) =
    let homeScore =
      Option.map (fun (ScoreLine (Score h, _)) -> h) fp.Prediction

    let awayScore =
      Option.map (fun (ScoreLine (_, Score a)) -> a) fp.Prediction

    let presetScoreButton s =
      Button.button [ Button.Size IsSmall
                      Button.Props [ Props.Style [ Width "4em" ] ] ] [
        str s
      ]

    [

      Text.div [ Modifiers [
                             Modifier.FlexWrap FlexWrap.NoWrap
                             Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        Option.map (ScoreBox.openScoreBox) fp.Prediction
        |> Option.defaultValue (ScoreBox.emptyScoreBox())
      ]
      Text.div [ Modifiers [
                             Modifier.FlexWrap FlexWrap.NoWrap
                             Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        scoreDecButton dispatch (fp, PredictTeam.Home, homeScore)
        scoreIncButton dispatch (fp, PredictTeam.Home)
        scoreIncButton dispatch (fp, PredictTeam.Away)
        scoreDecButton dispatch (fp, PredictTeam.Away, awayScore)
      ]
      Text.div [ Modifiers [
                             Modifier.FlexWrap FlexWrap.NoWrap
                             Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        presetScoreButton "4-2"
        presetScoreButton "3-2"
        presetScoreButton "2-2"
        presetScoreButton "2-3"
        presetScoreButton "2-4"
      ]

      Text.div [ Modifiers [
                             Modifier.FlexWrap FlexWrap.NoWrap
                             Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        presetScoreButton "3-1"
        presetScoreButton "2-1"
        presetScoreButton "1-1"
        presetScoreButton "1-2"
        presetScoreButton "1-3"
      ]

      Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Row
                             Modifier.FlexWrap FlexWrap.NoWrap
                             Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        presetScoreButton "2-0"
        presetScoreButton "1-0"
        presetScoreButton "0-0"
        presetScoreButton "0-1"
        presetScoreButton "0-2"
      ]
      doubleDownButton dispatch isDoubleDownAvailable fp ]

  let inplayFixtureModalContent dispatch (fp: FixturePredictionViewModel) = [ str "in play" ]

  let classifiedFixtureModalContent dispatch (fp: FixturePredictionViewModel) (sl) =
    [ Components.ScoreBox.resultScoreBox sl
      str "classified"
      // str (sprintf "points %i" points)
      // str (sprintf "%A" rc)
      ]


  let fixtureModal
    dispatch
    (GameweekNo gwno)
    isDoubleDownAvailable
    (fixtures: Map<FixtureId, FixturePredictionViewModel>)
    (modalState)
    =

    match modalState with
    | ModalOpen fId ->

      let ({ TeamLine = TeamLine (ht, at)
             Neighbours = (prev, next) } as fp: FixturePredictionViewModel) =
        fixtures.Item fId

      Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick(fun _ -> dispatch HideModal) ] []
        Modal.content [] [
          div [ Class "gw-fixture-modal-container" ] [
            Heading.h4 [ Heading.CustomClass "gw-fixture-modal-title page-title" ] [
              span [] [
                str (sprintf "Gameweek %i" gwno)
              ]
            ]

            div [ Class "gw-fixture-modal-content" ] [
              Components.shortTeamName ht
              Components.badge L ht
              Components.badge L at
              Components.shortTeamName at

              div
                []
                (fixtureSwitch
                  dispatch
                  fp
                  (openFixtureModalContent isDoubleDownAvailable)
                  inplayFixtureModalContent
                  classifiedFixtureModalContent)

            ]
            div [] [
              pageFixtureButton dispatch "<<" fixtures prev
              pageFixtureButton dispatch ">>" fixtures next
            ]

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
      fixtureModal dispatch gwfs.GameweekNo gwfs.IsDoubleDownAvailable gwfs.Fixtures modalState
    ]

  let view (model: Model) dispatch =
    match model.GameweekFixtures with
    | Success gwfs -> fullView dispatch gwfs model.ModalState
    | WebError e -> div [] [ str "error" ]
    | _ -> div [] []

  let updateSingleModelGwf model fId f =
    { model with
        GameweekFixtures =
          model.GameweekFixtures
          |> WebData.map
               (fun gwf ->
                 { gwf with
                     Fixtures = gwf.Fixtures.Change(fId, Option.map f) }) }

  let updateAllModelGwf model f =
    { model with
        GameweekFixtures =
          model.GameweekFixtures
          |> WebData.map
               (fun gwf ->
                 { gwf with
                     Fixtures = Map.map (fun _ v -> f v) gwf.Fixtures }) }

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | GameweekFixturesReceived r ->
      { model with
          GameweekFixtures = resultToWebData r },
      []
    | ShowModal fId ->
      { model with
          ModalState = ModalOpen fId },
      []
    | HideModal -> { model with ModalState = ModalClosed }, []
    | NavTo r -> model, (Routes.navTo r)

    | Prediction (PredictionAction (_, fId, _, _) as action) ->
      updateSingleModelGwf model fId (fun fixture -> { fixture with InProgress = true }),
      Cmd.OfAsync.perform (api.prediction player.Token) action PredictionAccepted
    | PredictionAccepted result ->
      match result with
      | Ok (PredictionAction (_, fId, team, vec)) ->

        let slfunc =
          match team, vec with
          | Home, Inc -> fun (ScoreLine (Score h, a)) -> ScoreLine(h + 1 |> Score, a)
          | Home, Dec -> fun (ScoreLine (Score h, a)) -> ScoreLine(h - 1 |> Score, a)
          | Away, Inc -> fun (ScoreLine (h, Score a)) -> ScoreLine(h, a + 1 |> Score)
          | Away, Dec -> fun (ScoreLine (h, Score a)) -> ScoreLine(h, a - 1 |> Score)

        let m =
          updateSingleModelGwf
            model
            fId
            (fun f ->
              { f with
                  Prediction =
                    Option.defaultValue ScoreLine.Init f.Prediction
                    |> slfunc
                    |> Some
                  InProgress = false })

        m, []
      | Error e -> updateAllModelGwf model (fun f -> { f with InProgress = false }), alert e
    | SetDoubleDown (fsId, fId) ->
      model, Cmd.OfAsync.perform (api.doubleDown player.Token) (fsId, fId) SetDoubleDownResponse
    | SetDoubleDownResponse r ->
      match r with
      | Ok (_, fId) ->
        let m =
          updateAllModelGwf model (fun f -> { f with IsDoubleDown = false })

        updateSingleModelGwf m fId (fun f -> { f with IsDoubleDown = true }), infoAlert "Double Down set"
      | Error e -> model, alert e
    | RemoveDoubleDown fsId ->
      model, Cmd.OfAsync.perform (api.removeDoubleDown player.Token) fsId RemoveDoubleDownResponse
    | RemoveDoubleDownResponse r ->
      match r with
      | Ok _ -> updateAllModelGwf model (fun f -> { f with IsDoubleDown = false }), infoAlert "Double Down removed"
      | Error e -> model, alert e
