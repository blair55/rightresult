namespace Areas.Gameweek

open Elmish
open Shared
open Routes
open Fable.Core
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
      Player: ClientSafePlayer
      ModalState: ModalState }

  type Msg =
    | Init of Result<string, exn>
    | Noop
    | GameweekFixturesReceived of Rresult<GameweekFixturesViewModel>
    | ShowModal of FixtureId
    | HideModal
    | NavTo of Route
    | Prediction of FixtureId * PredictionAction
    | PredictionAccepted of Rresult<PredictionAction>
    | SetDoubleDown of FixtureSetId * FixtureId
    | SetDoubleDownResponse of Rresult<FixtureSetId * FixtureId>
    | RemoveDoubleDown of FixtureSetId
    | RemoveDoubleDownResponse of Rresult<FixtureSetId>
    | ExpandBigUp of FixtureId
    | CollapseBigUp of FixtureId
    | SetBigUp of FixtureSetId * FixtureId
    | SetBigUpResponse of Rresult<FixtureSetId * FixtureId>

  let init api p gwno =
    { GameweekFixtures = Fetching
      Player = p
      ModalState = ModalClosed },
    Cmd.OfAsync.either (api.getGameweekFixtures p.Token) gwno GameweekFixturesReceived (Error >> Init)


  let xLargeTeamBadge = Components.badge Components.BadgeSize.XL

  let largeTeamBadge = Components.badge Components.BadgeSize.L

  let teamName (Team team) = str (team)
  // str (badgeAbbrv team)

  let resultClock (mp: string) =
    div [ Class "gw-item-clock" ] [ str mp ]

  let resultPoints (p, _) =
    div [ Class "gw-item-points" ] [
      pluralPoints p
    ]

  let gameweekItem dispatch ({ TeamLine = TeamLine (h, a) as tl } as fp: FixturePredictionViewModel) =

    let inplayClass =
      match fp.State with
      | FixtureState.InPlay _ -> "gw-item-inplay"
      | _ -> ""

    div [ Class "gw-item" ] [
      Components.GameweekItemTitle.element (fp.KickOff, tl)
      div [ Class $"gw-item-space {inplayClass}" ] []
      div [ Class "gw-item-body"
            OnClick(fun _ -> dispatch (ShowModal fp.Id)) ] [
        div [ Class $"gw-item-prediction {Components.predictionModifierClass fp.Prediction}" ] [
          div [ Class "gw-item-badges" ] [
            largeTeamBadge h
            largeTeamBadge a
          ]
          div [ Class "gw-item-score-polygon" ] [
            Components.PredictionScore.element fp.Prediction (fst fp.Points)
          ]
          div [ Class "gw-item-pointer" ] [
            Fa.i [ Fa.Solid.ChevronRight ] []
          ]
        ]
        div [ Class "gw-item-result" ] [
          div [ Class "gw-item-badges" ] [
            largeTeamBadge h
            largeTeamBadge a
          ]
          div [ Class "gw-item-score-polygon" ] [
            Components.PredictionScore.ResultScore.element fp.State
          ]
          div
            []
            (match fp.State with
             | FixtureState.Open _ -> []
             | FixtureState.InPlay (_, MinutesPlayed mp) -> [ resultClock mp ]
             | FixtureState.Classified _ ->
               [ resultClock "FT"
                 resultPoints fp.Points ])
        ]
      ]
    ]

  let fixtureGroup dispatch (_, fixtures) =
    div [ Class "gw-fixture-group" ] [
      div [ Class "gw-fixture-list" ] (fixtures |> List.map (gameweekItem dispatch))
    ]

  let button atts onClick content =
    Button.button
      ([ Button.IsFullWidth
         Button.OnClick onClick ]
       @ atts)
      content

  let isBigUp (f: FixturePredictionViewModel) =
    Option.map (snd >> PredictionModifier.isBigUp) f.Prediction
    |> Option.defaultValue false

  let scoreIncButton dispatch (f: FixturePredictionViewModel, team) =
    button
      [ Button.Color IsPrimary
        Button.IsOutlined
        Button.Disabled(isBigUp f) ]
      (if f.InProgress then
         ignore
       else
         fun _ ->
           (f.Id, PredictionAction.IncrementScore(f.FixtureSetId, f.Id, team))
           |> Prediction
           |> dispatch)
      [ Fa.i [ Fa.Solid.PlusSquare ] [] ]

  let scoreDecButton dispatch (f: FixturePredictionViewModel, team, score: int option) =
    match score with
    | Some s when s > 0 ->
      button
        [ Button.Color IsPrimary
          Button.IsOutlined
          Button.Disabled(isBigUp f) ]
        (if f.InProgress then
           ignore
         else
           fun _ ->
             (f.Id, PredictionAction.DecrementScore(f.FixtureSetId, f.Id, team))
             |> Prediction
             |> dispatch)
        [ Fa.i [ Fa.Solid.MinusSquare ] [] ]
    | _ -> button [ Button.Disabled true ] ignore [ Fa.i [ Fa.Solid.MinusSquare ] [] ]

  let doubleDownButton dispatch (model: GameweekFixturesViewModel) (f: FixturePredictionViewModel) =
    let icon i =
      [ Fa.i [ i ] []
        span [ Style [ MarginLeft "5px" ] ] [
          str "Double Down"
        ] ]

    match model.IsDoubleDownAvailable, f.Prediction, f.BigUpState with
    | _, _, BigUpState.Expanded -> div [] []

    | true, Some (_, PredictionModifier.DoubleDown), _ ->
      button
        [ Button.Color IsWarning ]
        (if f.InProgress then
           ignore
         else
           fun _ -> RemoveDoubleDown f.FixtureSetId |> dispatch)
        (icon Fa.Solid.CheckDouble)

    | true, Some (_, PredictionModifier.None), _ ->
      button
        [ Button.Color IsWarning
          Button.IsLight
          Button.IsOutlined ]
        (if f.InProgress then
           ignore
         else
           fun _ -> SetDoubleDown(f.FixtureSetId, f.Id) |> dispatch)
        (icon Fa.Solid.AngleDoubleDown)

    | _ -> button [ Button.Disabled true ] ignore (icon Fa.Solid.AngleDoubleDown)

  let bigUpButton dispatch (model: GameweekFixturesViewModel) (f: FixturePredictionViewModel) =
    let icon i txt =
      [ Fa.i [ i ] []
        span [ Style [ MarginLeft "5px" ] ] [
          str txt
        ] ]

    let bold s = b [] [ str s ]

    match f.Prediction, f.BigUpState with
    | Some (_, PredictionModifier.None), BigUpState.Available ->
      button
        [ Button.Color IsWarning
          Button.IsLight
          Button.IsOutlined ]
        (fun _ -> dispatch (ExpandBigUp f.Id))
        (icon Fa.Solid.AngleDoubleUp "Big Up")
    | Some (_, PredictionModifier.None), BigUpState.Expanded ->
      div [] [
        div [ Class "block" ] [
          Message.message [ Message.Color IsInfo ] [
            Message.body [ Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [
              Content.content [] [
                li [] [
                  bold "BIG UP"
                  str " on a correct score for "
                  bold "+5 points"
                ]
                li [] [
                  bold "BIG UP"
                  str " on a correct result for "
                  bold "+3 points"
                ]
                li [] [
                  bold "BIG UPs"
                  str " are visible to all players"
                ]
                li [] [
                  bold "BIG UPs"
                  str " cannot be edited"
                ]
                li [] [
                  str "One "
                  bold "BIG UP"
                  str " per gameweek"
                ]
              ]
            ]
          ]
        ]
        div [ Class "block" ] [
          button
            [ Button.Color IsInfo
              Button.IsLight
              Button.IsOutlined ]
            (fun _ -> dispatch (CollapseBigUp f.Id))
            (icon Fa.Solid.Times "Cancel")
        ]
        div [ Class "block" ] [
          button
            [ Button.Color IsWarning
              Button.IsLight
              Button.IsOutlined ]
            (if f.InProgress then
               ignore
             else
               fun _ -> SetBigUp(f.FixtureSetId, f.Id) |> dispatch)
            (icon Fa.Solid.AngleDoubleUp "Confirm Big Up")
        ]
      ]

    | Some (_, PredictionModifier.BigUp), _ -> button [ Button.Color IsWarning ] ignore (icon Fa.Solid.Lock "Big Up")

    | _ -> button [ Button.Disabled true ] ignore (icon Fa.Solid.AngleDoubleUp "Big Up")

  let bigUps dispatch (fp: FixturePredictionViewModel) =
    match fp.BigUpState, fp.FixtureDetails with
    | BigUpState.Expanded, _
    | _, None
    | _, Some { BigUps = [] } -> div [] []
    | _, Some { BigUps = bigUps } ->
      div [ Class "is-clearfix" ] [
        div [ Id "modal-big-ups"
              Class "big-up-box-container hide-scrollbars" ] [
          div [ Class "big-up-box-wrapper" ] (List.map (Components.bigUpBox (NavTo >> dispatch)) bigUps)
        ]
      ]

  let openFixtureModalContent dispatch (model: GameweekFixturesViewModel) (fp: FixturePredictionViewModel) =
    let homeScore = Option.map (fun (ScoreLine (Score h, _), _) -> h) fp.Prediction

    let awayScore = Option.map (fun (ScoreLine (_, Score a), _) -> a) fp.Prediction

    let presetScoreButton (sl: ScoreLine) =

      let highlightAttribute =
        match fp.Prediction with
        | Some (p, _) when p = sl -> []
        | _ -> [ Button.IsOutlined ]

      Button.button
        (highlightAttribute
         @ [ Button.Size IsSmall
             Button.Color IsPrimary
             Button.IsFullWidth
             Button.Disabled(isBigUp fp)
             Button.Props [ OnClick (fun _ ->
                              dispatch (Prediction(fp.Id, PredictionAction.SetScoreline(fp.FixtureSetId, fp.Id, sl)))) ] ])
        [ Components.simpleScore sl ]

    [ div
        []
        (fp.PredictionGrid
         |> List.map (fun x -> div [ Class "gw-fixture-preset-score-row" ] (List.map presetScoreButton x)))
      div [ Class "block" ] [
        div [ Class "gw-fixture-preset-score-row gw-fixture-incdec-dscore-row" ] [
          scoreIncButton dispatch (fp, PredictTeam.Home)
          scoreDecButton dispatch (fp, PredictTeam.Home, homeScore)
          scoreDecButton dispatch (fp, PredictTeam.Away, awayScore)
          scoreIncButton dispatch (fp, PredictTeam.Away)
        ]
      ]
      div [ Class "block" ] [
        doubleDownButton dispatch model fp
      ]
      div [ Style [ MarginBottom "2em" ]
            Class "block" ] [
        bigUpButton dispatch model fp
      ]
      div [ Class "block" ] [
        bigUps dispatch fp
      ] ]

  let matrixLink dispatch (GameweekNo gwno) =
    [ Message.message [ Message.Color IsInfo
                        Message.Modifiers [ Modifier.IsMarginless ] ] [
        Message.body [ Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [
          Content.content [] [
            str "See all predictions in the "
            a
              (anchorNavProps (NavTo >> dispatch) (LeaguesRoute(LeagueMatrixRoute(Global.identifier, gwno))))
              [ str (sprintf "Gameweek %i Matrix" gwno) ]
          ]
        ]
      ] ]

  let classifiedFixtureModalContent (fp: FixturePredictionViewModel) =
    [ Components.PointVectors.element fp.Points ]

  let pageFixtureButton dispatch icon fixtures =
    Option.bind (fun fId -> Map.tryFind fId fixtures)
    >> function
      | Some (fp: FixturePredictionViewModel) ->
        Button.button [ Button.Color IsLight
                        Button.Props [ OnClick(fun _ -> dispatch (ShowModal fp.Id)) ] ] [
          Fa.i [ icon ] []
        ]
      | None ->
        Button.button [ Button.Disabled true ] [
          Fa.i [ icon ] []
        ]

  let formGuideResult =
    function
    | FormResult.W -> b [] [ str "W" ]
    | FormResult.D -> b [] [ str "D" ]
    | FormResult.L -> b [] [ str "L" ]

  let formGuideVenue =
    function
    | FormVenue.H -> b [] [ str "H" ]
    | FormVenue.A -> b [] [ str "A" ]

  let formGuideElements venueClass ({ TeamLine = (TeamLine (h, a) as tl) } as f: FormFixture) =
    let formGuideResultClass =
      match f.Result with
      | FormResult.W -> "formguide-result-w"
      | FormResult.D -> "formguide-result-d"
      | FormResult.L -> "formguide-result-l"

    [ div [ Class $"formguide-team {formGuideResultClass} {venueClass}" ] [
        formGuideResult f.Result
        span [] [ str (badgeAbbrv h) ]
        div [ Class "formguide-scoreline" ] [
          simpleScore f.Scoreline
        ]
        span [] [ str (badgeAbbrv a) ]
        div [ Class "formguide-gwno" ] [
          span [] [
            str (GameweekNo.toGWString f.GameweekNo)
          ]
        ]
      ] ]

  let modalTitle dispatch ({ TeamLine = TeamLine (Team home, Team away) } as fp: FixturePredictionViewModel) =
    div [ Class "gw-fixture-modal-title" ] [
      Components.pageTitle (GameweekNo.toGWStringLong fp.GameweekNo)
      div [ Class "gw-fixture-modal-title-kotime" ] [
        str $"{fp.KickOff.ShortDay} {fp.KickOff.ClockTime}"
      ]
      div [ Class "gw-fixture-modal-title-kodate" ] [
        str fp.KickOff.DateAndShortMonth
      ]
      div [ Class "gw-fixture-modal-close"
            OnClick(fun _ -> dispatch HideModal) ] [
        Fa.i [ Fa.Solid.TimesCircle ] []
      ]
    ]

  let rowOf2 left right =
    Columns.columns [ Columns.IsMobile
                      Columns.IsGapless
                      Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ]
          Column.Width(Screen.All, Column.IsHalf) ]
        left
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Center ]
          Column.Width(Screen.All, Column.IsHalf) ]
        right
    ]

  let rowOf4 one two three four =
    Columns.columns [ Columns.IsMobile
                      Columns.IsGapless
                      Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ]
          Column.Width(Screen.All, Column.IsOneQuarter) ]
        one
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Left ]
          Column.Width(Screen.All, Column.IsOneQuarter) ]
        two
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Right ]
          Column.Width(Screen.All, Column.IsOneQuarter) ]
        three
      Column.column
        [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ]
          Column.Width(Screen.All, Column.IsOneQuarter) ]
        four
    ]

  let minsOrClassified =
    function
    | FixtureState.Open _ -> []
    | FixtureState.InPlay (_, MinutesPlayed mp) -> [ str mp ]
    | FixtureState.Classified _ -> [ str "FT" ]

  let modalBadgesAndScore ({ TeamLine = TeamLine (home, away) } as fp: FixturePredictionViewModel) =
    div [ Style [ MarginBottom "1em"
                  Position PositionOptions.Relative ] ] [
      rowOf4
        [ div [ Class "gw-fixture-modal-prediction" ] [
            Components.PredictionScore.element fp.Prediction (fst fp.Points)
          ] ]
        [ div [ Class "gw-fixture-modal-badge hometeam" ] [
            xLargeTeamBadge home
          ] ]
        [ div [ Class "gw-fixture-modal-badge" ] [
            xLargeTeamBadge away
          ] ]
        [ div [ Class "gw-fixture-modal-prediction" ] [
            Components.PredictionScore.ResultScore.element fp.State
            div [ Class "mins" ] (minsOrClassified fp.State)
          ] ]
    ]

  let formGuideRow left right =
    Columns.columns [ Columns.IsMobile
                      Columns.IsGapless
                      Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
      Column.column
        [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.RowReverse ]
          Column.Width(Screen.All, Column.IsHalf) ]
        left
      Column.column
        [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.Row ]
          Column.Width(Screen.All, Column.IsHalf) ]
        right
    ]

  let fixtureModal dispatch (model: GameweekFixturesViewModel) modalState =

    match modalState with
    | ModalOpen fId ->

      let ({ Neighbours = (prev, next) } as fp: FixturePredictionViewModel) =
        model.Fixtures.Item fId

      let matrixLink =
        Box.box' [ Modifiers [ Modifier.IsMarginless ] ] (matrixLink dispatch model.GameweekNo)

      let body, matrixLink =
        match fp.State with
        | FixtureState.Open _ -> openFixtureModalContent dispatch model fp, div [] []
        | FixtureState.InPlay _ -> [], matrixLink
        | FixtureState.Classified _ -> classifiedFixtureModalContent fp, matrixLink

      let formGuide (fp: FixturePredictionViewModel) =
        fp.FixtureDetails
        |> Option.mapF (fun fd -> fd.FormGuide) []
        |> List.map (fun (h, a) ->
          div [ Class "formguide-row" ] [
            formGuideRow
              (Option.mapF (formGuideElements "formguide-home") [] h)
              (Option.mapF (formGuideElements "formguide-away") [] a)
          ])
        |> div [ Class "formguide" ]

      Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
        Modal.Card.card [] [
          Modal.Card.body [ Props [ Id "fixture-modal-card" ] ] [
            div [ Class "gw-fixture-modal-container" ] [
              div [ Class "gw-fixture-modal-title-container" ] [
                modalTitle dispatch fp
                formGuide fp
              ]
              div [ Class "gw-fixture-modal-content" ] [
                Box.box' [] [
                  modalBadgesAndScore fp
                  div [] body
                ]
              ]
            ]
          ]
          matrixLink
          Modal.Card.foot [] [
            pageFixtureButton dispatch Fa.Solid.AngleDoubleLeft model.Fixtures prev
            pageFixtureButton dispatch Fa.Solid.AngleDoubleRight model.Fixtures next
          ]
        ]
      ]

    | ModalClosed -> div [] []

  let fullView
    dispatch
    ({ GameweekNo = GameweekNo gwno
       Neighbours = neighbours } as gwfs: GameweekFixturesViewModel)
    player
    modalState
    =
    div [ Class "gw-fixture" ] [
      Components.pageTitle (sprintf "Gameweek %i" gwno)

      gwfs.GlobalGameweekStats
      |> Option.mapF (Components.GameweekStats.element (NavTo >> dispatch) player) (div [] [])

      div
        []
        (Map.toList gwfs.Fixtures
         |> List.map snd
         |> List.sortBy (fun f -> f.SortOrder)
         |> List.groupBy (fun f -> f.KickOff.Group)
         |> List.map (fixtureGroup dispatch))

      div [ Class "gw-fixture-page-row" ] [
        Components.pageGwButtonRow
          (fun (GameweekNo gwno) ->
            NavTo(GameweekRoute(GameweekFixturesRoute gwno))
            |> dispatch)
          neighbours
      ]
      fixtureModal dispatch gwfs modalState
    ]

  let view (model: Model) dispatch =
    match model.GameweekFixtures with
    | Success gwfs -> fullView dispatch gwfs model.Player model.ModalState
    | WebError e -> div [] [ str "error" ]
    | _ -> div [] []

  let updateSingleModelGwf model fId f =
    { model with
        GameweekFixtures =
          model.GameweekFixtures
          |> WebData.map (fun gwf -> { gwf with Fixtures = gwf.Fixtures.Change(fId, Option.map f) }) }

  let updateAllModelGwf model f =
    { model with
        GameweekFixtures =
          model.GameweekFixtures
          |> WebData.map (fun gwf -> { gwf with Fixtures = Map.map (fun _ v -> f v) gwf.Fixtures }) }

  let removeDoubleDownFromAllPredictions model =
    updateAllModelGwf model (fun f ->
      { f with
          Prediction =
            f.Prediction
            |> Option.map (fun (sl, modifier) ->
              if PredictionModifier.isDoubleDown modifier then
                sl, PredictionModifier.None
              else
                sl, modifier)
          InProgress = false })

  let updatePredictionInModel model fId slfunc =
    updateSingleModelGwf model fId (fun f ->
      let p =
        f.Prediction
        |> Option.defaultValue (ScoreLine.Init, PredictionModifier.None)
        |> slfunc

      { f with
          Prediction = Some p
          PredictionGrid = PredictionGrid.redraw (fst p) f.PredictionGrid
          InProgress = false })

  let update api player msg model : Model * Cmd<Msg> =
    (match msg with
     | Init _ -> model, []
     | GameweekFixturesReceived r -> { model with GameweekFixtures = resultToWebData r }, []
     | ShowModal fId ->
       let m =
         updateSingleModelGwf model fId (fun fixture ->
           { fixture with
               BigUpState =
                 if fixture.BigUpState = BigUpState.Expanded then
                   BigUpState.Available
                 else
                   fixture.BigUpState })

       { m with ModalState = ModalOpen fId },
       Cmd.batch [ Cmd.OfFunc.perform Html.clip () (fun _ -> Noop)
                   Cmd.OfFunc.perform Html.resetScrollTop "fixture-modal-card" (fun _ -> Noop)
                   Cmd.OfFunc.perform Html.resetScrollLeft "modal-big-ups" (fun _ -> Noop) ]

     | HideModal -> { model with ModalState = ModalClosed }, Cmd.OfFunc.perform Html.unClip () (fun _ -> Noop)
     | Noop -> model, []
     | NavTo r ->
       model,
       Cmd.batch [ Routes.navTo r
                   Cmd.OfFunc.perform Html.unClip () (fun _ -> Noop) ]

     | Prediction (fId, action) ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with InProgress = true }),
       Cmd.OfAsync.perform (api.prediction player.Token) action PredictionAccepted

     | PredictionAccepted result ->
       match result with
       | Ok (PredictionAction.SetScoreline (_, fId, sl)) -> updatePredictionInModel model fId (fun (_, m) -> sl, m), []
       | Ok (PredictionAction.IncrementScore (_, fId, Home)) ->
         updatePredictionInModel model fId (fun (ScoreLine (Score h, a), m) -> ScoreLine(h + 1 |> Score, a), m), []
       | Ok (PredictionAction.DecrementScore (_, fId, Home)) ->
         updatePredictionInModel model fId (fun (ScoreLine (Score h, a), m) -> ScoreLine(h - 1 |> Score, a), m), []
       | Ok (PredictionAction.IncrementScore (_, fId, Away)) ->
         updatePredictionInModel model fId (fun (ScoreLine (h, Score a), m) -> ScoreLine(h, a + 1 |> Score), m), []
       | Ok (PredictionAction.DecrementScore (_, fId, Away)) ->
         updatePredictionInModel model fId (fun (ScoreLine (h, Score a), m) -> ScoreLine(h, a - 1 |> Score), m), []
       | Error e -> updateAllModelGwf model (fun f -> { f with InProgress = false }), alert e


     | SetDoubleDown (fsId, fId) ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with InProgress = true }),
       Cmd.OfAsync.perform (api.doubleDown player.Token) (fsId, fId) SetDoubleDownResponse
     | SetDoubleDownResponse r ->
       match r with
       | Ok (_, fId) ->
         let m = removeDoubleDownFromAllPredictions model
         updatePredictionInModel m fId (fun (sl, _) -> sl, PredictionModifier.DoubleDown), infoAlert "Double Down set"
       | Error e -> model, alert e
     | RemoveDoubleDown fsId ->
       updateAllModelGwf model (fun fixture -> { fixture with InProgress = true }),
       Cmd.OfAsync.perform (api.removeDoubleDown player.Token) fsId RemoveDoubleDownResponse
     | RemoveDoubleDownResponse r ->
       match r with
       | Ok _ -> removeDoubleDownFromAllPredictions model, infoAlert "Double Down removed"
       | Error e -> model, alert e

     | ExpandBigUp fId ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with BigUpState = BigUpState.Expanded }), []
     | CollapseBigUp fId ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with BigUpState = BigUpState.Available }), []
     | SetBigUp (fsId, fId) ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with InProgress = true }),
       Cmd.OfAsync.perform (api.bigUp player.Token) (fsId, fId) SetBigUpResponse
     | SetBigUpResponse r ->
       match r with
       | Ok (_, fId) ->
         let m =
           updateAllModelGwf model (fun f ->
             { f with
                 BigUpState = BigUpState.Unavailable
                 InProgress = false })

         let m =
           updateSingleModelGwf m fId (fun fixture ->
             { fixture with
                 BigUpState = BigUpState.Set
                 FixtureDetails =
                   (fixture.FixtureDetails, fixture.Prediction)
                   ||> Option.map2 (fun fd (sl, _) ->
                     { fd with
                         BigUps =
                           { PlayerName = PlayerName player.Name
                             PlayerId = player.Id
                             TeamLine = fixture.TeamLine
                             ScoreLine = sl }
                           :: fd.BigUps })
                 Prediction =
                   fixture.Prediction
                   |> Option.map (fun (sl, _) -> sl, PredictionModifier.BigUp) })

         m, infoAlert "Big Up!"
       | Error e -> model, alert e

    )
// |> (fun (({ GameweekFixtures = gwfs }, _) as r) ->
//   match gwfs with
//   | WebData.Success f ->
//     Map.toList f.Fixtures
//     |> List.map snd
//     |> List.sortBy (fun f -> f.SortOrder)
//     |> List.iter (fun f -> printfn "%A %A %A" f.TeamLine f.Prediction f.BigUpState)
//     |> ignore
//   | _ -> ()

//   r)