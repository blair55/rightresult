namespace Areas.Players

open Elmish

open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma

open Shared
open Areas
open Routes
open Elmish.ReactNative
open Elmish.React

module PlayerGameweek =

  type Model =
    { FixtureSet: PlayerGameweekViewModel WebData
      Player: ClientSafePlayer }

  type Msg =
    | Init of Result<string, exn>
    | FixtureSetReceived of Rresult<PlayerGameweekViewModel>
    | NavTo of Route
    | ToggleExpand of FixtureId
    | ExpandAll
    | CollapseAll

  let init api p playerId gwno =
    { FixtureSet = Fetching; Player = p },
    Cmd.OfAsync.either (api.getPlayerGameweek playerId gwno) p.Token FixtureSetReceived (Error >> Init)

  let rowOf2 class' left right =
    div [ Class class' ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.FlexEnd ]
            Column.Width(Screen.All, Column.IsHalf) ]
          left
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.SpaceBetween ]
            Column.Width(Screen.All, Column.IsHalf) ]
          right
      ]
    ]

  let gameweekItem
    dispatch
    areAnyItemsCollapsed
    ({ TeamLine = TeamLine (h, a) as tl } as fp: PlayerGameweekViewModelRow)
    =

    let mediumTeamBadge = Components.badge Components.BadgeSize.M

    let totalPoints =
      div [ Class "player-gw-total-points"
            OnClick(fun _ -> dispatch (ToggleExpand fp.FixtureId)) ] [
        Components.pluralPoints (fst fp.Points)
      ]

    let exColAllButton =
      if areAnyItemsCollapsed then
        div [ Class "player-gw-fixture-expandcollapse-all"
              OnClick(fun _ -> dispatch ExpandAll) ] [
          span [] [ str "expand all" ]
          Fa.i [ Fa.Solid.AngleDoubleDown
                 Fa.Size Fa.FaSmall ] []
        ]
      else
        div [ Class "player-gw-fixture-expandcollapse-all"
              OnClick(fun _ -> dispatch CollapseAll) ] [
          span [] [ str "collapse all" ]
          Fa.i [ Fa.Solid.AngleDoubleUp
                 Fa.Size Fa.FaSmall ] []
        ]

    let expandCollapse =
      match fp.State, fp.IsExpanded with
      | FixtureState.Classified _, false ->
        rowOf2
          "player-gw-fixture-expandcollapse"
          [ div [ OnClick(fun _ -> dispatch (ToggleExpand fp.FixtureId)) ] [
              span [] [ str "expand" ]
              Fa.i [ Fa.Solid.AngleDown
                     Fa.Size Fa.FaSmall ] []
            ] ]
          [ totalPoints; exColAllButton ]

      | FixtureState.Classified _, true ->
        rowOf2
          "player-gw-fixture-expandcollapse"
          [ div [ OnClick(fun _ -> dispatch (ToggleExpand fp.FixtureId)) ] [
              span [] [ str "collapse" ]
              Fa.i [ Fa.Solid.AngleUp; Fa.Size Fa.FaSmall ] []
            ] ]
          [ totalPoints; exColAllButton ]

      | _ -> div [] []

    let expandCollapseClass =
      if fp.IsExpanded then
        ""
      else
        "gw-item-collapsed"

    div [ Class(sprintf "gw-item %s" expandCollapseClass) ] [
      Components.GameweekItemTitle.element (fp.KickOff, tl)
      div [ Class "gw-item-body" ] [
        div [ Class $"gw-item-prediction {Components.predictionModifierClass fp.Prediction}" ] [
          div [ Class "gw-item-score-polygon" ] [
            mediumTeamBadge h
            Components.PredictionScore.element fp.Prediction (fst fp.Points)
            mediumTeamBadge a
          ]
        ]
        div [ Class "gw-item-result" ] [
          div [ Class "gw-item-score-polygon" ] [
            mediumTeamBadge h
            Components.PredictionScore.ResultScore.element fp.State
            mediumTeamBadge a
          ]
        ]
      ]
      Components.PointVectors.element fp.Points
      expandCollapse
    ]

  let fixtureGroup dispatch areAnyItemsCollapsed (_, fixtures) =
    div [ Class "gw-fixture-group" ] [
      div
        [ Class "gw-fixture-list" ]
        (fixtures
         |> List.map (gameweekItem dispatch areAnyItemsCollapsed))
    ]

  let gwStats dispatch player =
    Option.mapF (Components.GameweekStats.element (NavTo >> dispatch) player) (div [] [])

  let gwPageButtonRow nav { PlayerGameweekViewModel.Neighbours = neighbours } =
    div [ Class "gw-fixture-page-row" ] [
      Components.pageGwButtonRow nav neighbours
    ]

  let areAnyItemsCollapsed =
    List.filter (fun (f: PlayerGameweekViewModelRow) -> FixtureState.isClassified f.State)
    >> List.exists (fun f -> not f.IsExpanded)

  let fullView
    dispatch
    ({ GameweekNo = gwno
       PlayerName = PlayerName name
       PlayerId = PlayerId playerId } as gwfs: PlayerGameweekViewModel)
    player
    =
    let rows =
      Map.toList gwfs.Fixtures
     |> List.map snd

    div [ Class "player-gw-fixture" ] [
      Components.pageTitle (name + " / " + GameweekNo.toGWString gwno)
      gwStats dispatch player gwfs.GlobalGameweekStats
      div
        []
        (rows
         |> List.sortBy (fun f -> f.SortOrder)
         |> List.groupBy (fun f -> f.KickOff.Group)
         |> List.map (fixtureGroup dispatch (areAnyItemsCollapsed rows)))
      gwPageButtonRow
        (fun (GameweekNo gwno) ->
          NavTo(PlayersRoute(PlayerGameweekRoute(playerId, gwno)))
          |> dispatch)
        gwfs
    ]

  let view (model: Model) dispatch =
    match model.FixtureSet with
    | Success fs -> fullView dispatch fs model.Player
    | _ -> div [] []

  let updateSingleModelGwf (model: Model) fId f =
    { model with
        FixtureSet =
          model.FixtureSet
          |> WebData.map
               (fun fs ->
                 { fs with
                     Fixtures = fs.Fixtures.Change(fId, Option.map f) }) }

  let updateAllModelGwf model f =
    { model with
        FixtureSet =
          model.FixtureSet
          |> WebData.map
               (fun gwf ->
                 { gwf with
                     Fixtures = Map.map (fun _ v -> f v) gwf.Fixtures }) }

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | FixtureSetReceived r ->
      { model with
          FixtureSet = resultToWebData r },
      []
    | NavTo r -> model, (Routes.navTo r)
    | ToggleExpand fId -> updateSingleModelGwf model fId (fun r -> { r with IsExpanded = not r.IsExpanded }), []
    | ExpandAll -> updateAllModelGwf model (fun r -> { r with IsExpanded = true }), []
    | CollapseAll -> updateAllModelGwf model (fun r -> { r with IsExpanded = false }), []
