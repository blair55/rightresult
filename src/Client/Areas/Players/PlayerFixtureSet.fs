namespace Areas.Players

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Fable
open Areas
open Routes
open Components
open Elmish.ReactNative
open Elmish.React

module PlayerFixtureSet =

  type Model =
    { FixtureSet : PlayerFixtureSetViewModel WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | FixtureSetReceived of Rresult<PlayerFixtureSetViewModel>
    | NavTo of Route

  let init api p playerId fsId =
    { FixtureSet = Fetching
    },
      Cmd.OfAsync.either
        (api.getPlayerFixtureSet playerId fsId)
        p.Token
        FixtureSetReceived
        (Error >> Init)

  let rowOf4 one two three four =
    div []
      [ Columns.columns [ Columns.IsMobile; Columns.IsGapless]
          [ Column.column
              [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Right) ]
                Column.Width (Screen.Desktop, Column.Is4)
                Column.Width (Screen.Tablet, Column.Is4)
                Column.Width (Screen.Mobile, Column.Is3)
              ] one
            Column.column
              [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                Column.Width (Screen.Desktop, Column.Is2)
                Column.Width (Screen.Tablet, Column.Is2)
                Column.Width (Screen.Mobile, Column.Is3)
              ] two
            Column.column
              [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ]
                Column.Width (Screen.Desktop, Column.Is4)
                Column.Width (Screen.Tablet, Column.Is4)
                Column.Width (Screen.Mobile, Column.Is3)
              ] three
            Column.column
              [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                Column.Width (Screen.Desktop, Column.Is2)
                Column.Width (Screen.Tablet, Column.Is2)
                Column.Width (Screen.Mobile, Column.Is3)
              ] four
          ]
      ]

  let fixtureSetView (row:PlayerFixtureSetKickedOffViewModelRow) =
    let (TeamLine (home, away)) =
      row.TeamLine
    let fixtureResultScoreBox =
      match row.ResultAndPoints with
      | Some (scoreLine, _) ->
        ScoreBox.resultScoreBox scoreLine
      | None ->
        ScoreBox.emptyScoreBox()
    let predictionScoreBox =
      match row.ResultAndPoints, row.Prediction with
      | Some (_, cat), Some (sl, dd) ->
        ScoreBox.classifiedScoreBox sl dd cat
      | None, Some (sl, dd) ->
        ScoreBox.kickedOffScoreBox sl dd
      | _ ->
        ScoreBox.emptyScoreBox()
    let homeTeam =
      div [ Class "player-fixtureset-team home" ]
        [ Components.teamNameNotOnMobile home
          div [ Class "badge" ] [ Components.badge M home ]
        ]
    let awayTeam =
      div [ Class "player-fixtureset-team away" ]
        [ div [ Class "badge" ] [ Components.badge M away ]
          Components.teamNameNotOnMobile away
        ]
    rowOf4
      [ homeTeam ]
      [ fixtureResultScoreBox ]
      [ awayTeam ]
      [ predictionScoreBox ]
    |> fun r ->
    Card.card []
      [ Card.content [ Props [ Style [ Padding "0.6em 0em" ] ] ]
          [ r ]
      ]

  let fixtureGroup (KickOffString koStr, fixtures) =
    div [ Style [ MarginBottom "1.5em" ] ]
      [ Components.subHeading koStr
        div [] (fixtures |> List.map fixtureSetView)
      ]

  let totalPoints (fixtures:PlayerFixtureSetViewModel) =
    Components.card
      [ Components.pointsTotalView fixtures.TotalPoints
      ]

  let fullView dispatch (fixtures:PlayerFixtureSetViewModel) =
    let (GameweekNo gwno) = fixtures.GameweekNo
    let (PlayerName name) = fixtures.PlayerName
    div []
      [ Components.pageTitle name
        Components.subHeading <| sprintf "Gameweek %i" gwno
        totalPoints fixtures
        fixtures.Rows
        |> List.groupBy (fun r -> r.KickOffString)
        |> List.map fixtureGroup
        |> div [ Class "player-fixtureset" ]
      ]

  let view (model:Model) dispatch =
    match model.FixtureSet with
    | Success fs -> fullView dispatch fs
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | FixtureSetReceived r -> { model with FixtureSet = resultToWebData r }, []
    | NavTo r -> model, (Routes.navTo r)
