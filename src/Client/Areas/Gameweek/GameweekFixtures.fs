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
      ModalState = ModalClosed },
    Cmd.OfAsync.either (api.getGameweekFixtures p.Token) gwno GameweekFixturesReceived (Error >> Init)




  // let fixtureScoreBox { FixturePredictionViewModel.State = state; TeamLine = TeamLine (home, away) } =
  //   div [ Class "gw-fixture-result" ] [
  //     match state with
  //     | FixtureState.Open ko ->
  //       div [ Class "gw-fixture-result-clock" ] [ str (ko.Raw.ToString("HH:mm")) ]
  //       div [ Class "gw-fixture-result-box" ] [
  //         // div [Class""] [ str (badgeAbbrv home) ]
  //         // div [Class""] [ str (badgeAbbrv away) ]
  //       ]
  //     | FixtureState.InPlay (ScoreLine (Score h, Score a), MinutesPlayed mp) ->
  //       div [ Class "gw-fixture-result-box" ] [
  //         div [Class""] [ str (string<int> h) ]
  //         div [Class""] [ str (string<int> a) ]
  //       ]
  //       div [ Class "gw-fixture-result-clock" ] [ str mp ]
  //     | FixtureState.Classified (ScoreLine (Score h, Score a)) ->
  //       div [ Class "gw-fixture-result-box" ] [
  //         div [Class""] [ str (string<int> h) ]
  //         div [Class""] [ str (string<int> a) ]
  //       ]
  //       div [ Class "gw-fixture-result-clock" ] [ str "FT" ]
  //   ]

  let predScoreEmpty =
    div [ Class "pred-score-empty" ] [ str "" ]

  let predScore (s:string) =
    div [ Class "pred-score" ] [ str s ]

  let predScoreline (ScoreLine (Score h, Score a)) =
    [ predScore (string<int> h); predScore (string<int> a) ]

  let predDoubleDown =
    div [ Class "pred-dd" ] [
      Fa.i [ Fa.Solid.AngleDoubleDown
             Fa.Size Fa.FaSmall ] []
    ]

  let predBigUp =
    div [ Class "pred-bigup" ] [
      Fa.i [ Fa.Solid.AngleDoubleUp
             Fa.Size Fa.FaSmall ] []
    ]

  let predScoreContainer pred =
    match pred with
    | Some (sl, PredictionModifier.BigUp) ->
        predScoreline sl
          @ [ div [ Class "pred-bigup" ] [
              Fa.i [ Fa.Solid.AngleDoubleUp
                     Fa.Size Fa.FaSmall ] []
            ]
        ]
    | Some (sl, PredictionModifier.DoubleDown) ->
        predScoreline sl
          @ [ div [ Class "pred-dd" ] [
              Fa.i [ Fa.Solid.AngleDoubleDown
                     Fa.Size Fa.FaSmall ] []
            ]
        ]
    | Some (sl, PredictionModifier.None) ->
        predScoreline sl
    | None ->
        [ predScoreEmpty; predScoreEmpty ]
    |> div [Class "pred-score-container"]


  // let predictionScoreBox = function
  //     | Some (ScoreLine (Score h, Score a), PredictionModifier.DoubleDown) ->
  //       div [ Class "gw-fixture-pred-box" ] [
  //         div [ Class "pred-dd" ] [
  //           Fa.i [ Fa.Solid.AngleDoubleDown
  //                  Fa.Size Fa.FaSmall ] []
  //         ]
  //         div [ Class "pred-score" ] [
  //           str (string<int> h)
  //         ]
  //         div [ Class "pred-score" ] [
  //           str (string<int> a)
  //         ]
  //       ]
  //     | Some (ScoreLine (Score h, Score a), PredictionModifier.BigUp) ->
  //       div [ Class "gw-fixture-pred-box" ] [
  //         div [ Class "pred-bigup" ] [
  //           Fa.i [ Fa.Solid.AngleDoubleUp
  //                  Fa.Size Fa.FaSmall ] []
  //         ]
  //         div [ Class "pred-score" ] [
  //           str (string<int> h)
  //         ]
  //         div [ Class "pred-score" ] [
  //           str (string<int> a)
  //         ]
  //       ]
  //     | Some (ScoreLine (Score h, Score a), _) ->
  //       div [ Class "gw-fixture-pred-box" ] [
  //         div [ Class "pred-score" ] [
  //           str (string<int> h)
  //         ]
  //         div [ Class "pred-score" ] [
  //           str (string<int> a)
  //         ]
  //       ]
  //     | None ->
  //       div [ Class "gw-fixture-pred-box" ] [
  //         div [ Class "pred-score" ] [ str ("_") ]
  //         div [ Class "pred-score" ] [ str ("_") ]
  //       ]

  // let pointerBox { FixturePredictionViewModel.State = state; Prediction = pred; Points = (p, _) } =
  //     match state, pred with
  //     | FixtureState.Open _, None ->
  //       div [Class "gw-fixture-result-pointer predict"] [
  //         span [ ] [ str "predict" ]
  //         Fa.i [ Fa.Solid.AngleRight ] []
  //       ]
  //     | FixtureState.Classified _, _ ->
  //       div [Class "gw-fixture-result-pointer points"] [
  //         Components.pluralPoints p
  //         Fa.i [ Fa.Solid.AngleRight ] []
  //       ]
  //     | _ ->
  //       div [Class "gw-fixture-result-pointer"] [ ]

  // let fixtureItemSwitch
  //   dispatch
  //   ({ TeamLine = TeamLine (home, away)
  //      Prediction = pred } as fp: FixturePredictionViewModel)
  //   =

    // let resultClass =
    //   match fp.State with
    //   | FixtureState.Open _ -> "gw-fixture-open"
    //   | FixtureState.InPlay _ -> "gw-fixture-inplay"
    //   | FixtureState.Classified _ -> "gw-fixture-classified"

    // let predClass =
    //   if Option.isSome pred then
    //     "gw-fixture-haspred"
    //   else
    //     "gw-fixture-nopred"

    // let modifierClass =
    //   Option.map (fun (_, modifier) -> PredictionModifier.isModified modifier) pred
    //   |> Option.defaultValue false
    //   |> function
    //     | true -> "gw-fixture-item-ismodified"
    //     | _ -> ""

    // Text.div [ Props [ Class $"gw-fixture-item {modifierClass}"
    //                    OnClick(fun _ -> dispatch (ShowModal fp.Id)) ] ] [
      // div [ Class "bg-badge-container" ] [
      //     Components.bigBackgroundBadge home
      //     Components.bigBackgroundBadge away
      // ]

      // rowOf3
      //   $"gw-fixture-result-row {resultClass}"
      //   [ fixtureBadge home ]
      //   [ div [] [ fixtureScoreBox fp ] ]
      //   [ fixtureBadge away ]
      // rowOf3
      //   $"gw-fixture-pred-row {predClass}"
      //   [ Components.shortTeamName home ]
      //   [ predictionScoreBox pred ]
      //   [ Components.shortTeamName away ]

      // rowOf3
      //   // "gw-fixture-result-row"
      //   // [ div [ Class "gw-fixture-item-team" ] [ fixtureBadge home; Components.shortTeamName home ] ]
      //   [ div [ Class "gw-fixture-item-team" ] [ fixtureBadge home ] ]
      //   [ div [ Class "gw-fixture-item-core" ] [ fixtureScoreBox fp; predScoreContainer pred; ] ]
      //   [ div [ Class "gw-fixture-item-team" ] [ fixtureBadge away ] ]
      //   // [ div [ Class "gw-fixture-item-team" ] [ fixtureBadge away; Components.shortTeamName away ] ]

    // ]

  let xLargeTeamBadge =
    Components.badge Components.BadgeSize.XL

  let largeTeamBadge =
    Components.badge Components.BadgeSize.L

  let teamName (Team team) =
    str (team)
    // str (badgeAbbrv team)

  let resultClock (mp:string) =
    div [Class "gw-item-clock"] [ str mp ]

  let resultPoints (p, _) =
    div [ Class "gw-item-points"] [ pluralPoints p ]

  let resultContainer (fp : FixturePredictionViewModel) =
    div [Class "pred-score-container"]
      (match fp.State with
      | FixtureState.Open _ -> [ predScore "_"; predScore "_" ]
      | FixtureState.InPlay (sl, _)
      | FixtureState.Classified sl -> predScoreline sl)

  let predictionModifierClass =
    Option.map (fun (_:ScoreLine, modifier) -> PredictionModifier.isModified modifier)
    >> Option.defaultValue false
    >> function
      | true -> "gw-item-ismodified"
      | _ -> ""

  let kickOffClockTime (ko:KickOff) =
    str (ko.Raw.ToString("HH:mm"))

  let gameweekItem dispatch
    ({ TeamLine=TeamLine (h, a) } as fp: FixturePredictionViewModel) =

    div [Class "gw-item" ]
      [ div [ Class "gw-item-title"]
          [ div [Class "gw-item-gameday"] [ str (fp.KickOffShortDay) ]
            div [Class "gw-item-team"] [ teamName h ]
            div [Class "gw-item-ko"] [ kickOffClockTime fp.KickOff ]
            div [Class "gw-item-team"] [ teamName a ]
          ]
        div [ Class "gw-item-space"]
          []
        div [Class"gw-item-body"; OnClick(fun _ -> dispatch (ShowModal fp.Id)) ]
          [ div [Class $"gw-item-prediction {predictionModifierClass fp.Prediction}"]
              [ div [Class "gw-item-badges"]
                  [ largeTeamBadge h
                    largeTeamBadge a
                  ]
                div [Class "gw-item-score-polygon"]
                  [ predScoreContainer fp.Prediction
                  ]
                div [ Class "gw-item-pointer"]
                  [ Fa.i [ Fa.Solid.ChevronRight ] []
                  ]
              ]
            div [Class "gw-item-result"]
              [ div [Class "gw-item-badges"]
                  [ largeTeamBadge h
                    largeTeamBadge a
                  ]
                div [Class "gw-item-score-polygon"]
                  [ resultContainer fp ]
                div []
                  (match fp.State with
                  | FixtureState.Open _ -> []
                  | FixtureState.InPlay (_, MinutesPlayed mp) -> [ resultClock mp ]
                  | FixtureState.Classified _ -> [ resultClock "FT"; resultPoints fp.Points ])
              ]
          ]
      ]

  let fixtureGroup dispatch (koStr, fixtures) =
    div [ Class "gw-fixture-group" ] [
      // div [ Style [ MarginBottom "2em" ] ] [
      //   Components.gameweekDate koStr
      // ]
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
                Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [
                  str "Big Up for more points!"
                ]
                li [] [
                  str "Big Ups are visible to all players"
                ]
                li [] [ str "Big Ups cannot be edited" ]
                li [] [ str "One Big Up per gameweek" ]
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
        div [ Class "big-up-box-container hide-scrollbars" ] [
          div [ Class "big-up-box-wrapper" ] (List.map (Components.bigUpBox (NavTo >> dispatch)) bigUps)
        ]
      ]

  let openFixtureModalContent dispatch (model: GameweekFixturesViewModel) (fp: FixturePredictionViewModel) =
    let homeScore =
      Option.map (fun (ScoreLine (Score h, _), _) -> h) fp.Prediction

    let awayScore =
      Option.map (fun (ScoreLine (_, Score a), _) -> a) fp.Prediction

    let presetScoreButton homeScore awayScore =
      let sl =
        ScoreLine(Score homeScore, Score awayScore)

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
             Button.Props [ OnClick
                              (fun _ ->
                                dispatch (Prediction(fp.Id, PredictionAction.SetScoreline(fp.FixtureSetId, fp.Id, sl)))) ] ])
        [ Components.simpleScore sl ]

    [ div [ Class "" ] [
        div [ Class "gw-fixture-preset-score-row" ] [
          presetScoreButton 2 0
          presetScoreButton 1 0
          presetScoreButton 0 0
          presetScoreButton 0 1
          presetScoreButton 0 2
        ]

        div [ Class "gw-fixture-preset-score-row" ] [
          presetScoreButton 3 1
          presetScoreButton 2 1
          presetScoreButton 1 1
          presetScoreButton 1 2
          presetScoreButton 1 3
        ]
        div [ Class "gw-fixture-preset-score-row" ] [
          presetScoreButton 4 2
          presetScoreButton 3 2
          presetScoreButton 2 2
          presetScoreButton 2 3
          presetScoreButton 2 4
        ]
      ]
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

  // let fixtureStateLabel state =
  //   div [ Class "gw-fixture-modal-state" ] [
  //     Columns.columns [ Columns.IsMobile
  //                       Columns.IsGapless
  //                       Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
  //       Column.column [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Center ]
  //                       Column.Width(Screen.All, Column.IsFull) ] [
  //         span [] [ str state ]
  //       ]
  //     ]
  //   ]

  let inplayFixtureModalContent dispatch (GameweekNo gwno) (sl, mp) =
    [ Message.message [ Message.Color IsInfo ] [
        Message.body [ Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [
          Content.content [] [
            str "See all predictions in the "
            a
              (anchorNavProps (NavTo >> dispatch) (LeaguesRoute(LeagueMatrixRoute(Global.identifier, gwno))))
              [ str (sprintf "gameweek %i matrix" gwno) ]
          ]
        ]
      ] ]

  let vectorRow left right =
    div [ Class "point-vector-row" ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.RowReverse ]
            Column.Width(Screen.All, Column.IsHalf) ]
          left
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Left ]
            Column.Width(Screen.All, Column.IsHalf) ]
          right
      ]
    ]

  let (|VectorDesc|DoubleDownDesc|) =
    function
    | PointVector.Result -> VectorDesc(str "Right Result", pluralPoints 2)
    | PointVector.HomeScore -> VectorDesc(str "Home Score", pluralPoints 1)
    | PointVector.AwayScore -> VectorDesc(str "Away Score", pluralPoints 1)
    | PointVector.GoalDifference -> VectorDesc(str "Goal Difference", pluralPoints 1)
    | PointVector.BigUp b -> VectorDesc(str "Big Up", pluralPoints b)
    | PointVector.DoubleDown -> DoubleDownDesc(str "Double Down")

  let vector =
    function
    | VectorDesc (desc, p) ->
      vectorRow [ div [ Class "point-vector-row-left" ] [
                    desc
                  ] ] [
        div [ Class "point-vector-row-right" ] [
          str "+ "
          p
        ]
      ]
    | DoubleDownDesc desc ->
      vectorRow [ div [ Class "point-vector-row-left" ] [
                    desc
                  ] ] [
        div [ Class "point-vector-row-right" ] [
          str "× 2"
        ]
      ]

  // let pointsHeader =
  //   div [ Class "point-vector-row-header" ] [
  //     vectorRow [ div [ Class "point-vector-row-left" ] [
  //                   str "score"
  //                 ] ] []
  //   ]

  let totalPoints p =
    div [ Class "point-vector-row-total" ] [
      vectorRow [] [
        div [ Class "point-vector-row-right" ] [
          pluralPoints p
        ]
      ]
    ]

  let classifiedFixtureModalContent dispatch ({ Points = (p, vts) } as fp: FixturePredictionViewModel) =
    let rows =
      vts
      |> List.map (vector)
      |> fun v -> v @ [ totalPoints p ]
    [ div [] rows ]

  let pageGwButton dispatch icon =
    function
    | Some (GameweekNo gwno) ->
      Button.button [ Button.Color IsLight
                      Button.Props [ OnClick(fun _ -> dispatch (NavTo(GameweekRoute(GameweekFixturesRoute gwno)))) ] ] [
        Fa.i [ icon ] []
      ]
    | None ->
      Button.button [ Button.Disabled true ] [
        Fa.i [ icon ] []
      ]

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

  let formGuideRow left right =
    div [ Class "gw-fixture-formguide-row" ] [
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
    ]

  let venueToString =
    function
    | FormVenue.H -> "H"
    | FormVenue.A -> "A"

  let formGuideResult =
    function
    | FormResult.W ->
      div [ Class "formguide-result-w" ] [
        span [] [ str "W" ]
      ]
    | FormResult.D ->
      div [ Class "formguide-result-d" ] [
        span [] [ str "D" ]
      ]
    | FormResult.L ->
      div [ Class "formguide-result-l" ] [
        span [] [ str "L" ]
      ]

  let formGuideElements ({ TeamLine = TeamLine (home, away) } as f: FormFixture) =
    [ formGuideResult f.Result
      div [ Class "formguide-team" ] [
        div [] [ str (badgeAbbrv home) ]
        div [ Class "formguide-scoreline" ] [
          simpleScore f.Scoreline
        ]
        div [] [ str (badgeAbbrv away) ]
      ] ]

  let modalTitle ({ TeamLine = TeamLine (Team home, Team away) } as fp: FixturePredictionViewModel) =
    let clockOrScore =
        match fp.State with
        | FixtureState.Open ko -> [ kickOffClockTime ko ]
        | FixtureState.InPlay (sl, _) -> [ Components.simpleScore sl ]
        | FixtureState.Classified sl -> [ Components.simpleScore sl ]
    let minsOrClassified =
        match fp.State with
        | FixtureState.Open _ -> []
        | FixtureState.InPlay (_, MinutesPlayed mp) -> [ str mp ]
        | FixtureState.Classified _ -> [ str "FT" ]

    div [ Class "gw-fixture-modal-title" ]
      [ div [Class "gw-fixture-modal-team"] [ str (home) ]
        div [Class "gw-fixture-modal-title-score" ] clockOrScore
        div [Class "gw-fixture-modal-team"] [ str (away) ]
        div [Class "gw-fixture-modal-title-minutes" ] minsOrClassified ]

  let rowOf2 left right =
    // div [ ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ]
            Column.Width(Screen.All, Column.IsHalf) ]
          left
        // Column.column
        //   [ Column.Modifiers [  Modifier.FlexDirection FlexDirection.Column
        //                         Modifier.FlexAlignItems FlexAlignItems.Center ]
        //     Column.Width(Screen.All, Column.IsOneThird) ]
        //   center
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ]
            Column.Width(Screen.All, Column.IsHalf) ]
          right
      // ]
    ]

  let modalBadgesAndScore ({ TeamLine = TeamLine (home, away) } as fp: FixturePredictionViewModel) =
    div [ Style [MarginBottom "1em"; Position PositionOptions.Relative ] ]
      [ rowOf2
          [ div [ Class "gw-fixture-modal-badge home-badge" ] [ xLargeTeamBadge home ]
            div [ Class "gw-fixture-modal-badge away-badge" ] [ xLargeTeamBadge away ] ]
          [ div [ Class "gw-fixture-modal-prediction" ] [ predScoreContainer fp.Prediction ] ]
      ]

  let fixtureModal dispatch (model: GameweekFixturesViewModel) modalState =

    match modalState with
    | ModalOpen fId ->

      let ({ Neighbours = (prev, next) } as fp: FixturePredictionViewModel) = model.Fixtures.Item fId

      let body =
        match fp.State with
        | FixtureState.Open _ -> openFixtureModalContent dispatch model fp
        | FixtureState.InPlay (sl, mp) -> inplayFixtureModalContent dispatch model.GameweekNo (sl, mp)
        | FixtureState.Classified _ -> classifiedFixtureModalContent dispatch fp
        |> Text.div [ Props [ Class "" ] ]

      let formGuide (fp: FixturePredictionViewModel) =
        fp.FixtureDetails
        |> Option.mapF (fun fd -> fd.FormGuide) []
        |> List.map
             (fun (h, a) -> formGuideRow (Option.mapF formGuideElements [] h) (Option.mapF formGuideElements [] a))
        |> function
          | [] -> div [] []
          | fg -> div [ Class "gw-fixture-formguide" ] fg

      Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick(fun _ -> dispatch HideModal) ] []
        Modal.Card.card [] [
          Modal.Card.body [] [
            div [ Class "gw-fixture-modal-container" ] [
              // div [ Style [ MarginBottom "2em" ] ] [
              //   Components.gameweekDate fp.KickOffGroup
              // ]
              modalTitle fp
              div [ Class "gw-fixture-modal-space"] []
              div [ Class "gw-fixture-modal-content" ] [
                // Box.box' [ Props [ Style [ PaddingTop "0" ] ] ] [
                Box.box' [ ] [
                  modalBadgesAndScore fp
                  formGuide fp
                  body
                ]
              ]
            ]
          ]
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
       Neighbours = prev, next } as gwfs: GameweekFixturesViewModel)
    modalState
    =
    div [ Class "gw-fixture" ] [
      Components.pageTitle (sprintf "Gameweek %i" gwno)
      div
        []
        (Map.toList gwfs.Fixtures
         |> List.map snd
         |> List.sortBy (fun f -> f.SortOrder)
         |> List.groupBy (fun f -> f.KickOffGroup)
         |> List.map (fixtureGroup dispatch))

      div [ Class "gw-fixture-page-row" ] [
        Box.box' [] [
          pageGwButton dispatch Fa.Solid.AngleDoubleLeft prev
          pageGwButton dispatch Fa.Solid.AngleDoubleRight next
        ]
      ]
      fixtureModal dispatch gwfs modalState
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

  let removeDoubleDownFromAllPredictions model =
    updateAllModelGwf
      model
      (fun f ->
        { f with
            Prediction =
              f.Prediction
              |> Option.map
                   (fun (sl, modifier) ->
                     if PredictionModifier.isDoubleDown modifier then
                       sl, PredictionModifier.None
                     else
                       sl, modifier)
            InProgress = false })

  let updatePredictionInModel model fId slfunc =
    updateSingleModelGwf
      model
      fId
      (fun f ->
        { f with
            Prediction =
              f.Prediction
              |> Option.defaultValue (ScoreLine.Init, PredictionModifier.None)
              // |> fun (sl, modifier) -> slfunc sl, modifier
              |> slfunc
              |> Some
            InProgress = false })

  let update api player msg model : Model * Cmd<Msg> =
    (match msg with
     | Init _ -> model, []
     | GameweekFixturesReceived r ->
       { model with
           GameweekFixtures = resultToWebData r },
       []
     | ShowModal fId ->
       let m =
         updateSingleModelGwf
           model
           fId
           (fun fixture ->
             { fixture with
                 BigUpState =
                   if fixture.BigUpState = BigUpState.Expanded then
                     BigUpState.Available
                   else
                     fixture.BigUpState })

       { m with ModalState = ModalOpen fId }, Cmd.OfFunc.perform Html.clip () (fun _ -> Noop)
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
       updateSingleModelGwf
         model
         fId
         (fun fixture ->
           { fixture with
               BigUpState = BigUpState.Expanded }),
       []
     | CollapseBigUp fId ->
       updateSingleModelGwf
         model
         fId
         (fun fixture ->
           { fixture with
               BigUpState = BigUpState.Available }),
       []
     | SetBigUp (fsId, fId) ->
       updateSingleModelGwf model fId (fun fixture -> { fixture with InProgress = true }),
       Cmd.OfAsync.perform (api.bigUp player.Token) (fsId, fId) SetBigUpResponse
     | SetBigUpResponse r ->
       match r with
       | Ok (_, fId) ->
         let m =
           updateAllModelGwf
             model
             (fun f ->
               { f with
                   BigUpState = BigUpState.Unavailable
                   InProgress = false })

         let m =
           updateSingleModelGwf
             m
             fId
             (fun fixture ->
               { fixture with
                   BigUpState = BigUpState.Set
                   FixtureDetails =
                     (fixture.FixtureDetails, fixture.Prediction)
                     ||> Option.map2
                           (fun fd (sl, _) ->
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
