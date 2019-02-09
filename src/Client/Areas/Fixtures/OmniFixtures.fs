namespace Areas.Fixtures

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared
open Fulma
open Fable.FontAwesome
open Fable
open Routes
open Areas
open Components

module OmniFixtures =

  let initFrom = 0
  let pageSize = 10

  type Model =
    { Page : Page
      Length : int
      Fixtures : FixtureMap
    }
  and FixtureMap =
    Map<FixtureId, FixturePredictionViewModel>
  and Page =
    { Current : int
      Prev : int option
      Next : int option
    }

  let buildPage current length =
    { Current = current
      Prev = if current + pageSize < length then Some (current + pageSize) else None
      Next = if current > 0 then Some (current - pageSize) else None
    }
    |> fun p -> printfn "%A" p; p

  type Msg =
    | Init of Rresult<Unit>
    | FixturesReceived of Rresult<FixtureMap>
    | FixturesLengthReceived of Rresult<int>
    | Prediction of PredictionAction
    | PredictionAccepted of Rresult<PredictionAction>
    | SetDoubleDown of FixtureSetId * FixtureId
    | SetDoubleDownResponse of Rresult<FixtureSetId * FixtureId>
    | RemoveDoubleDown of FixtureSetId
    | RemoveDoubleDownResponse of Rresult<FixtureSetId>
    | Page of from:int

  let getFixturesCmd api player from =
    Cmd.ofAsync
      (api.getFixtures (from, pageSize))
      player.Token
      FixturesReceived
      (AsyncError >> Error >> Init)

  let getFixturesLengthCmd api player =
    Cmd.ofAsync
      api.getFixturesLength
      player.Token
      FixturesLengthReceived
      (AsyncError >> Error >> Init)

  let init api player =
    { Page = buildPage initFrom 0
      Length = 0
      Fixtures = Map.empty
    }, Cmd.batch
        [ getFixturesCmd api player initFrom
          getFixturesLengthCmd api player
        ]

  let button atts onClick content =
    Button.button (
      [ Button.IsFullWidth
        Button.OnClick onClick ] @ atts)
      content

  let rowOf2 one two =
    div [ Class TextAlignment.Classes.HasTextCentered ]
      [ Columns.columns [ Columns.IsMobile; Columns.IsGapless ]
          [ Column.column [ Column.Width (Screen.All, Column.IsHalf) ] one
            Column.column [ Column.Width (Screen.All, Column.IsHalf) ] two
          ]
      ]

  let rowOf3 one two three =
    div [ Class TextAlignment.Classes.HasTextCentered ]
      [ Columns.columns [ Columns.IsMobile; Columns.IsGapless]
          [ Column.column [ Column.Width (Screen.All, Column.IsTwoFifths) ] one
            Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] two
            Column.column [ Column.Width (Screen.All, Column.IsTwoFifths) ] three
          ]
      ]

  let rowOf5 one two three four five =
    div [ Class TextAlignment.Classes.HasTextCentered ]
      [ Columns.columns [ Columns.IsMobile; Columns.IsGapless ]
          [ Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] one
            Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] two
            Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] three
            Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] four
            Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ] five
          ]
      ]

  let badgeAndScoreRow (f:FixturePredictionViewModel) predictionBox =
    let (TeamLine (homeTeam, awayTeam)) = f.TeamLine
    div [ Style [ Position "relative" ] ]
      [ div [ Style [ Position "relative" ] ]
          [ rowOf3
              [ div [ Style [ Margin "0 auto"; Display "inline-block" ] ] [ badge L homeTeam ] ]
              []
              [ div [ Style [ Margin "0 auto"; Display "inline-block" ] ] [ badge L awayTeam ] ]
            rowOf3
              [ Components.teamName homeTeam ] [] [ Components.teamName awayTeam ]
          ]
        div [ Style [ Position "absolute"; Width "100%"; Top "0.6em" ] ]
          [ rowOf3
              [ ]
              [ predictionBox ]
              [ ]
          ]
      ]

  let fixtureTitleBar (text, textColor, bgColor, isDoubleDown, rhs)  =
    Card.header []
      [ Card.Header.title
          [ Card.Header.Title.Modifiers
              [ Modifier.BackgroundColor bgColor
                Modifier.TextTransform TextTransform.UpperCase
                Modifier.TextSize (Screen.All, TextSize.Is7)
                Modifier.TextColor textColor
                Modifier.IsShadowless
              ]
            Card.Header.Title.Props
              [ Style
                  [ Padding "0.5em 1.4em"
                  ]
              ]
          ]
          [ Text.span [] [ str text ]

            (if isDoubleDown
            then
              Text.span []
                // [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.AngleDoubleDown ]
                //   str "Double Down"
                // ]
                // [ Fa.i [ Fa.Solid.AngleDoubleDown; Fa.Size Fa.FaSmall ] [ str "Double Down" ] ]
                [ smallIconWithText Fa.Solid.AngleDoubleDown "Double Down" ]

            else Text.span [] [])
            Text.span [] rhs
          ]
      ]

  let openFixtureView dispatch (f:FixturePredictionViewModel) =
    let (KickOff ko) = f.KickOff
    let homeScore = Option.map (fun (ScoreLine (Score h, _)) -> h) f.Prediction
    let awayScore = Option.map (fun (ScoreLine (_, Score a)) -> a) f.Prediction
    let footItem item = Card.Footer.a [] item

    let rhs =
      match homeScore, awayScore with
      | Some _, Some _ -> []
      | _ ->
        // [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.AngleDoubleRight ]
        //   str "Awaiting"
        // ]
        [ smallIconWithText Fa.Solid.AngleDoubleRight "Awaiting" ]

    let disabledIcon i =
      // Icon.faIcon
      //   [ Icon.Modifiers
      //       [ Modifier.TextColor IsGreyLight ]
      //   ] [ Fa.icon i ]
      Fa.i [ i; Fa.Props [ Style [ Color "#b5b5b5" ] ] ] []

    let scoreIncButton dispatch (fsId, fId, team) =
      button [ Button.Color IsLight ]
        (if f.InProgress then ignore else fun _ -> PredictionAction (fsId, fId, team, Inc) |> Prediction |> dispatch)
        [ Fa.i [ Fa.Solid.AngleUp ] [] ]

    let scoreDecButton dispatch (fsId, fId, team, score:int option) =
      match score with
      | Some s when s > 0 ->
        button [ Button.Color IsLight ]
          (if f.InProgress then ignore else fun _ -> PredictionAction (fsId, fId, team, Dec) |> Prediction |> dispatch)
          [ Fa.i [ Fa.Solid.AngleDown ] [] ]
      | _ -> disabledIcon Fa.Solid.AngleDown

    let doubleDownButton dispatch (f:FixturePredictionViewModel) =
      let icon i =
        // [ Icon.faIcon [] [ Fa.icon i ] ]
        [ Fa.i [ i ] [] ]
      match f.IsDoubleDownAvailable, f.Prediction, f.IsDoubleDown with
      | true, Some _, false -> button [Button.Color IsLight] (if f.InProgress then ignore else fun _ -> SetDoubleDown (f.FixtureSetId, f.Id) |> dispatch) (icon Fa.Solid.AngleDoubleDown)
      | true, Some _, true  -> button [Button.Color IsWarning] (if f.InProgress then ignore else fun _ -> RemoveDoubleDown f.FixtureSetId |> dispatch) (icon Fa.Solid.AngleDoubleDown)
      | _ -> disabledIcon Fa.Solid.AngleDoubleDown

    let predictionBox =
      match f.Prediction with
      | Some p -> ScoreBox.openScoreBox p
      | None -> ScoreBox.emptyScoreBox()

    [ fixtureTitleBar (ko.DateTime.ToString("HH:mm"), IsWhite, IsPrimary, f.IsDoubleDown, rhs)

      Card.content
        [ Props
            [ Style [ Position "relative"; Padding "1em 0" ]
            ]
        ]
        [ badgeAndScoreRow f predictionBox
        ]

      Card.footer []
        [ footItem
            [ scoreDecButton dispatch (f.FixtureSetId, f.Id, PredictTeam.Home, homeScore) ]
          footItem
            [ scoreIncButton dispatch (f.FixtureSetId, f.Id, PredictTeam.Home) ]
          footItem
            [ doubleDownButton dispatch f ]
          footItem
            [ scoreIncButton dispatch (f.FixtureSetId, f.Id, PredictTeam.Away) ]
          footItem
            [ scoreDecButton dispatch (f.FixtureSetId, f.Id, PredictTeam.Away, awayScore) ]
        ]
    ]

  let kickedOffFixtureView (f:FixturePredictionViewModel) =
    let predictionBox =
      match f.Prediction with
      | Some p -> ScoreBox.kickedOffScoreBox p f.IsDoubleDown
      | _ -> ScoreBox.emptyScoreBox()
    [ fixtureTitleBar ("In play", IsDark, IsWarning, f.IsDoubleDown, [])
      Card.content []
        [ badgeAndScoreRow f predictionBox
        ]
    ]

  let classifiedFixtureView
    (f:FixturePredictionViewModel)
    (ScoreLine (Score homeScore, Score awayScore), points, category) =
    let s = if points = 1 then sprintf "%i point" points else sprintf "%i points" points
    let rhs =
        // [ Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.AngleDoubleRight ]
        [ smallIconWithText Fa.Solid.AngleDoubleRight s
        ]
    let predictionBox =
      match f.Prediction with
      | Some p -> ScoreBox.classifiedScoreBox p f.IsDoubleDown category
      | _ -> ScoreBox.emptyScoreBox()
    [ fixtureTitleBar (sprintf "Result %i-%i" homeScore awayScore, IsWhite, IsInfo, f.IsDoubleDown, rhs)
      Card.content []
        [ badgeAndScoreRow f predictionBox
        ]
    ]

  let fixtureView dispatch (f:FixturePredictionViewModel) =
    div [ Style [ CSSProp.MarginBottom "1.5em" ] ]
      [ Card.card []
          (match f.State with
            | FixtureState.Open -> openFixtureView dispatch f
            | FixtureState.KickedOff -> kickedOffFixtureView f
            | FixtureState.Classified (result, points, category) ->
              classifiedFixtureView f (result, points, category))
      ]

  let groupedDateView dispatch ko (GameweekNo gwno) fixtures =
    div []
      [ div [ Style [ MarginBottom "1em"; MarginLeft "1em" ] ]
          [ Heading.h5 [ Heading.IsSubtitle ] [ str <| sprintf "%s • GW%i" ko gwno ] ]
        div [ Style [ MarginBottom "2em" ] ]
          (fixtures
          |> List.sortBy (fun (f:FixturePredictionViewModel) -> f.SortOrder)
          |> List.map (fixtureView dispatch))
      ]

  let view (model:Model) dispatch =
    let dateGroupedFixtures =
      Map.toList model.Fixtures
      |> List.groupBy (fun (_, { KickOff = (KickOff ko) }) -> ko.Date)
      |> List.sortBy (fun (date, _) -> date)
    div [ ]
      [ Components.pageTitle "Fixtures"
        div []
          (dateGroupedFixtures
          |> List.map (fun (_, items) ->
              let (_, first) = items.Head
              groupedDateView dispatch first.FormattedKickOff first.GameweekNo (List.map snd items)))

        rowOf2
            (match model.Page.Prev with
            | Some p ->
              [ button [ Button.Size IsSmall; Button.Modifiers [ Modifier.IsPulledLeft ] ]
                  (fun _ -> Page p |> dispatch)
                  [ Fa.i [ Fa.Solid.AngleRight ] []
                  // [ Icon.faIcon [] [ Fa.icon Fa.I.AngleLeft ]
                    str "Prev"
                  ]
              ]
            | None -> [])
            (match model.Page.Next with
            | Some n ->
              [ button [ Button.Size IsSmall; Button.Modifiers [ Modifier.IsPulledRight ] ]
                  (fun _ -> Page n |> dispatch)
                  [ str "Next"
                    // Icon.faIcon [] [ Fa.icon Fa.I.AngleRight ]
                    Fa.i [ Fa.Solid.AngleRight ] []
                  ]
              ]
            | None -> [])
      ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init r ->
      match r with
      | Ok _ -> model, []
      | Error e -> model, alert e
    | FixturesReceived r ->
      match r with
      | Ok fixtures -> { model with Fixtures = fixtures; Page = buildPage model.Page.Current model.Length }, []
      | Error e -> model, alert e
    | FixturesLengthReceived r ->
      match r with
      | Ok length -> { model with Length = length }, []
      | Error e -> model, alert e
    | Prediction action ->
      let (PredictionAction (_, fId, _, _)) = action
      { model with Fixtures = model.Fixtures.Add(fId, { model.Fixtures.Item fId with InProgress = true }) },
      Cmd.ofAsync
        (api.prediction player.Token)
        action
        PredictionAccepted
        (AsyncError >> Error >> Init)
    | PredictionAccepted result ->
      match result with
      | Ok (PredictionAction (_, fId, team, vec)) ->
        match team, vec with
        | Home, Inc -> fun (ScoreLine (Score h, a)) -> ScoreLine (h+1 |> Score, a)
        | Home, Dec -> fun (ScoreLine (Score h, a)) -> ScoreLine (h-1 |> Score, a)
        | Away, Inc -> fun (ScoreLine (h, Score a)) -> ScoreLine (h, a+1 |> Score)
        | Away, Dec -> fun (ScoreLine (h, Score a)) -> ScoreLine (h, a-1 |> Score)
        |> fun f ->
          model.Fixtures.Item fId
          |> fun { Prediction = sl } -> sl
          |> function
          | Some pred -> pred
          | None -> ScoreLine.Init
          |> f |> fun p -> { model with Fixtures = model.Fixtures.Add(fId, { model.Fixtures.Item fId with Prediction = Some p; InProgress = false }) }, []
      | Error e ->
        { model with Fixtures = Map.map (fun k v -> { v with InProgress = false }) model.Fixtures }, alert e
    | SetDoubleDown (fsId, fId) ->
      model,
      Cmd.ofAsync
        (api.doubleDown player.Token)
        (fsId, fId)
        SetDoubleDownResponse
        (AsyncError >> Error >> Init)
    | SetDoubleDownResponse r ->
      match r with
      | Ok (fsId, fId) ->
        model.Fixtures
        |> Map.map (fun _ f -> if f.FixtureSetId = fsId then { f with IsDoubleDown = false } else f)
        |> fun m -> { model with Fixtures = m.Add(fId, { m.[fId] with IsDoubleDown = true }) }, []
      | Error e -> model, alert e
    | RemoveDoubleDown fsId ->
      model,
      Cmd.ofAsync
        (api.removeDoubleDown player.Token)
        fsId
        RemoveDoubleDownResponse
        (AsyncError >> Error >> Init)
    | RemoveDoubleDownResponse r ->
      match r with
      | Ok fsId ->
        model.Fixtures
        |> Map.map (fun _ f -> if f.FixtureSetId = fsId then { f with IsDoubleDown = false } else f)
        |> fun m -> { model with Fixtures = m }, []
      | Error e -> model, alert e
    | Page from ->
      { model with Page = buildPage from model.Length }, getFixturesCmd api player from
