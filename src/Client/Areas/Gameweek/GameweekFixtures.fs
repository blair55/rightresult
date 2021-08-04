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

  let rowOf3 class' left center right =
    div [ Class class' ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.Row
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ]
            Column.Width(Screen.All, Column.IsTwoFifths) ]
          left
        Column.column
          [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.Row
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ]
            Column.Width(Screen.All, Column.IsOneFifth) ]
          center
        Column.column
          [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.Row
                               Modifier.FlexJustifyContent FlexJustifyContent.Center ]
            Column.Width(Screen.All, Column.IsTwoFifths) ]
          right
      ]
    ]

  let fixtureBadge team =
    div [] [
      Components.badge Components.BadgeSize.L team
    ]

  let fixtureScoreBox state =
    div [ Class "gw-fixture-result-container" ] [
      match state with
      | FixtureState.Open ko ->
        div [ Class "gw-fixture-result-desc" ] [
          span [] [
            str (ko.Raw.ToString("HH:mm"))
          ]
        ]

        div [ Class "gw-fixture-result-box" ] [
          div [] [ str ("_") ]
          div [] [ str ("_") ]
        ]
      | FixtureState.InPlay (ScoreLine (Score h, Score a), MinutesPlayed mp) ->
        div [ Class "gw-fixture-result-desc" ] [
          span [] [ str (string<int> mp + "'") ]
        ]

        div [ Class "gw-fixture-result-box" ] [
          div [] [ str (string<int> h) ]
          div [] [ str (string<int> a) ]
        ]
      | FixtureState.Classified (ScoreLine (Score h, Score a)) ->
        div [ Class "gw-fixture-result-desc" ] [
          span [] [ str ("FT") ]
        ]

        div [ Class "gw-fixture-result-box" ] [
          div [] [ str (string<int> h) ]
          div [] [ str (string<int> a) ]
        ]
    ]

  let predictionScoreBox pred =
    div [ Class "gw-fixture-pred-container" ] [
      match pred with
      | (Some (ScoreLine (Score h, Score a)), true) ->
        div [ Class "gw-fixture-pred-box" ] [
          div [ Class "pred-score" ] [
            str (string<int> h)
          ]
          div [ Class "pred-score" ] [
            str (string<int> a)
          ]
          div [ Class "pred-dd" ] [
            Fa.i [ Fa.Solid.AngleDoubleDown
                   Fa.Size Fa.FaSmall ] []
          ]

        ]
      | (Some (ScoreLine (Score h, Score a)), _) ->
        div [ Class "gw-fixture-pred-box" ] [
          div [ Class "pred-score" ] [
            str (string<int> h)
          ]
          div [ Class "pred-score" ] [
            str (string<int> a)
          ]
        ]
      | None, _ ->
        div [ Class "gw-fixture-pred-box" ] [
          div [ Class "pred-score" ] [ str ("_") ]
          div [ Class "pred-score" ] [ str ("_") ]
        ]
    ]

  let fixtureItemSwitch
    dispatch
    ({ TeamLine = TeamLine (home, away)
       IsDoubleDown = dd
       Prediction = pred } as fp: FixturePredictionViewModel)
    =

    let resultClass =
      match fp.State with
      | FixtureState.Open _ -> "gw-fixture-open"
      | FixtureState.InPlay _ -> "gw-fixture-inplay"
      | FixtureState.Classified _ -> "gw-fixture-classified"

    let predClass =
      if Option.isSome pred then
        "gw-fixture-haspred"
      else
        "gw-fixture-nopred"

    let ddClass = if dd then "gw-fixture-item-isdd" else ""

    Text.div [ Props [ Class $"gw-fixture-item {ddClass}"
                       OnClick(fun _ -> dispatch (ShowModal fp.Id)) ] ] [
      rowOf3
        $"gw-fixture-result-row {resultClass}"
        [ fixtureBadge home ]
        [ fixtureScoreBox fp.State ]
        [ fixtureBadge away ]
      rowOf3
        $"gw-fixture-pred-row {predClass}"
        [ Components.shortTeamName home ]
        [ predictionScoreBox (pred, dd) ]
        [ Components.shortTeamName away ]

      div [ Class "gw-fixture-item-arrow" ] [
        Fa.i [ Fa.Solid.AngleRight ] []
      ]
    ]

  let fixtureGroup dispatch (koStr, fixtures) =
    div [ Style [ MarginBottom "1.5em" ] ] [
      div [ Style [ MarginBottom "2em" ] ] [
        Components.gameweekDate koStr
      ]
      div [ Class "gw-fixture-list" ] (fixtures |> List.map (fixtureItemSwitch dispatch))
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

  let openFixtureModalContent dispatch isDoubleDownAvailable (fp: FixturePredictionViewModel) =
    let homeScore =
      Option.map (fun (ScoreLine (Score h, _)) -> h) fp.Prediction

    let awayScore =
      Option.map (fun (ScoreLine (_, Score a)) -> a) fp.Prediction

    let presetScoreButton s =
      Button.button [ Button.Size IsSmall
                      Button.Props [ Props.Style [ Width "4em" ] ] ] [
        str s
      ]

    [ Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        Option.map (ScoreBox.openScoreBox) fp.Prediction
        |> Option.defaultValue (ScoreBox.emptyScoreBox ())
      ]
      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        scoreDecButton dispatch (fp, PredictTeam.Home, homeScore)
        scoreIncButton dispatch (fp, PredictTeam.Home)
        scoreIncButton dispatch (fp, PredictTeam.Away)
        scoreDecButton dispatch (fp, PredictTeam.Away, awayScore)
      ]
      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
        presetScoreButton "4-2"
        presetScoreButton "3-2"
        presetScoreButton "2-2"
        presetScoreButton "2-3"
        presetScoreButton "2-4"
      ]

      Text.div [ Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.SpaceEvenly ] ] [
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

      let body =
        match fp.State with
        | FixtureState.Open _ -> openFixtureModalContent dispatch isDoubleDownAvailable fp
        | FixtureState.InPlay (sl, mp) -> inplayFixtureModalContent dispatch fp
        | FixtureState.Classified (sl) -> classifiedFixtureModalContent dispatch fp sl
        |> Text.div [ Props [ Class "" ] ]

      Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick(fun _ -> dispatch HideModal) ] []
        Modal.content [] [
          div [ Class "gw-fixture-modal-container" ] [
            div [ Style [ MarginBottom "2em" ] ] [
              Components.pageTitle (sprintf "Gameweek %i" gwno)
            ]

            div [ Class "gw-fixture-modal-content" ] [
              fixtureItemSwitch dispatch fp
              body
              div [] [
                pageFixtureButton dispatch "<<" fixtures prev
                pageFixtureButton dispatch ">>" fixtures next
              ]
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
         |> List.groupBy (fun f -> f.KickOffGroup)
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
