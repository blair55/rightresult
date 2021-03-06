namespace Areas.Fixtures

open Elmish
open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Fable.FontAwesome
open Areas
open Components

module FixtureDetails =

  let teamSection (col:FixtureDetailsColumn) =
    let row =
      col.PremTableRow
    let ordinal = function
      | 1 -> "st"
      | 2 -> "nd"
      | 3 -> "rd"
      | _ -> "th"
    let teamName (Team team) =
      Heading.h6 []
        [ str team
          str " • "
          span [Class "fd-position"] [str (string col.PremTableRow.Position)]
          span [Class "fd-ordinal"] [str (ordinal row.Position)]
        ]
    let titleRow =
      div [ Class (sprintf "fd-title team-title %s" (badgeAbbrv col.Team)) ]
        [ Columns.columns [ Columns.IsMobile; Columns.IsGapless ]
            [ Column.column [ Column.Width (Screen.All, Column.IsOneFifth) ]
                [ div [Class "fd-badge-container"]
                    [ div [Class "fd-badge"] [ badge L col.Team ] ]
                ]
              Column.column [ Column.Width (Screen.All, Column.IsFourFifths) ]
                [ div [Class "fd-team-name"]
                    [ teamName col.Team ]
                ]
            ]

        ]
    let tableRow =
      let cell k v =
        Column.column [ Column.Width (Screen.All, Column.Is2) ]
          [ span [ Class "fd-key" ]
              [ str k ]
            span [ Class "fd-val" ]
              [ str v ]
          ]
      div [Class "fd-league-row"]
        [ Columns.columns [ Columns.IsMobile; Columns.IsGapless ]
            [ cell "P" (string row.Played)
              cell "W" (string row.Won)
              cell "D" (string row.Drawn)
              cell "L" (string row.Lost)
              cell "GD" ((row.GoalsFor - row.GoalsAgainst) |> signedInt)
              cell "Pts" (string row.Points)
            ]
        ]

    let take n =
      List.truncate n
      >> List.map Some
      >> fun l ->
        let diff = n - List.length l
        if diff > 0 then
          (l, [1..diff]) ||> List.fold (fun  state _  -> state @ [ None ])
        else l

    let formGuideGoal = function
      | Some _ -> div [ Class "fg-goal fg-goal-some" ] []
      | None -> div [ Class "fg-goal fg-goal-none" ] []

    let formGuideResult result venue =
      match result with
      | FormResult.W -> Hero.hero [ Hero.IsBold; Hero.Color IsSuccess ] [ div [ Class "fg-result-w" ] [ span [] [ str "W" ] ] ]
      | FormResult.D -> div [ Class "fg-result-d" ] [ span [] [ str "D" ] ]
      | FormResult.L -> Hero.hero [ Hero.IsBold; Hero.Color IsDanger ] [ div [ Class "fg-result-l" ] [ span [] [ str "L" ] ] ]

    let formGuideVenue = function
      | H -> div [ Class "fg-venue-h" ] [ span [] [ str "home" ] ]
      | A -> div [ Class "fg-venue-a" ] [ span [] [ str "away" ] ]

    let formGuideColumn maxGf maxGa opacity
      { FormFixture.GoalsFor = Score goalsFor
        GoalsAgainst = Score goalsAgainst
        Result = result
        Venue = venue } =
      Column.column [ Column.Width (Screen.All, Column.Is2) ]
        [ div [Class "fg-column"; Style [ Opacity ((string opacity))] ]
            [ div [] [ formGuideVenue venue ]
              div [ Class "fg-goals-for" ] ([0..goalsFor-1] |> take 5 |> List.rev |> List.map formGuideGoal)
              div [] [ formGuideResult result venue ]
              div [ Class "fg-goals-against" ] ([0..goalsAgainst-1] |> take 5 |> List.map formGuideGoal)
            ]
        ]

    let formGuideColumnNoFixture =
      Column.column [ Column.Width (Screen.All, Column.Is2) ]
        [ div [Class "fg-column"]
            [ div [Class "fg-no-fixture"]
                []
            ]
        ]

    let formGuideRow =
      let colCount = 6
      div [ Class "form-guide" ]
        [ Columns.columns [ Columns.IsMobile; Columns.IsGapless ]
            (col.FormGuide
            |> take colCount
            |> List.rev
            |> (fun l ->
              let maxGf = l |> List.map (function | Some { GoalsFor = Score gf } -> gf | None -> 0) |> List.max
              let maxGa = l |> List.map (function | Some { GoalsAgainst = Score ga } -> ga | None -> 0) |> List.max
              l
              |> List.mapi (fun i col ->
                match col, i with
                | Some col, 0 -> formGuideColumn maxGf maxGa 0.40 col
                | Some col, 1 -> formGuideColumn maxGf maxGa 0.70 col
                | Some col, 2 -> formGuideColumn maxGf maxGa 0.90 col
                | Some col, _ -> formGuideColumn maxGf maxGa 1.00 col
                | _ -> formGuideColumnNoFixture)))
        ]

    div [ Class "fixture-details" ]
      [ titleRow
        formGuideRow
        tableRow
      ]
