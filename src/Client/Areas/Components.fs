namespace Areas

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared
open Fulma

module CustomClasses =

  [<Literal>]
  let TextRight = "has-text-right"

  [<Literal>]
  let TextLeft = "has-text-left"

  [<Literal>]
  let TextCenter = "has-text-centered"

module Components =

  let signedInt i =
    if i > 0 then
      sprintf "%+i" i
    else
      string i

  let heroBar =
    Hero.hero [ Hero.Color IsPrimary
                Hero.Props [ Style [ MarginBottom "1em" ] ] ] [
      Hero.body [] [
        Container.container [] [
          Heading.h1 [ Heading.Is3
                       Heading.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ] ] [
            span [] [ str "Right Result" ]
          ]
          Heading.h3 [ Heading.IsSubtitle; Heading.Is6 ] [
            Fa.i [ Fa.Solid.AngleDoubleDown ] []
            str " 2021/22"
          ]
        ]
      ]
    ]

  let pointsTotalView (total: PredictionPointsMonoid) =
    let brokenSpan one two =
      [ span [] [ str one ]
        br []
        span [] [ str two ] ]

    Level.level [ Level.Level.Option.IsMobile ] [
      Level.item [ Level.Item.HasTextCentered ] [
        div [] [
          Level.heading [] (brokenSpan "correct" "results")
          Level.title [] [
            str (
              string (
                total.CorrectResults
              // + total.DoubleDownCorrectResults
              )
            )
          ]
        ]
      ]
      Level.item [ Level.Item.HasTextCentered ] [
        div [] [
          Level.heading [] (brokenSpan "correct" "scores")
          Level.title [] [
            str (
              string (
                total.CorrectScores
              // + total.DoubleDownCorrectScores
              )
            )
          ]
        ]
      ]
      Level.item [ Level.Item.HasTextCentered ] [
        div [] [
          Level.heading [] (brokenSpan "total" "points")
          Level.title [] [
            str (string total.Points)
          ]
        ]
      ]
    ]

  let pageTitle s =
    // div [ Style [ MarginLeft "1em"; MarginTop "1em"; MarginBottom "1em" ] ]
    //   [ Heading.h1 [ Heading.Is3 ] [ str s ] ]
    Heading.h1 [ Heading.Props [ Class "page-title" ] ] [
      span [] [ str s ]
    ]

  let subHeading s =
    // Heading.h5 [ Heading.IsSubtitle
    //              Heading.Props [ Style [ MarginLeft "1em" ] ] ] [
    //   str s
    // ]
    Heading.h2 [ Heading.Props [ Class "page-sub-title" ] ] [
      span [] [ str s ]
    ]

  let teamName (Team team) =
    Text.span [ Modifiers [ Modifier.TextSize(Screen.Mobile, TextSize.Is7) ] ] [
      str team
    ]

  let teamNameNotOnMobile (Team team) =
    Text.span [ Modifiers [ Modifier.IsHidden(Screen.Mobile, true) ] ] [
      str team
    ]

  let pluralPoints p =
    if p = 1 then "pt" else "pts"
    |> sprintf "%i %s" p
    |> str

  let card content =
    div [ Style [ MarginBottom "1em" ] ] [
      Card.card [ Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
        Card.content [] content
      ]
    ]

  let cardWithFooter content footer =
    div [ Style [ MarginBottom "1em" ] ] [
      Card.card [ Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
        Card.content [] content
        Card.footer [] footer
      ]
    ]

  let toShortPoints (m: PredictionPointsMonoid) =
    // m.Points, m.CorrectResults + m.DoubleDownCorrectResults, m.CorrectScores + m.DoubleDownCorrectScores
    m.Points, m.CorrectResults, m.CorrectScores

  let table (league: LeagueTableDoc) activePlayerId (playerClick: PlayerId -> Unit) =
    let playerLink pId (m: LeagueTableMember) =
      let (PlayerName name) = m.PlayerName

      a [ OnClick(fun _ -> playerClick pId) ] [
        str name
      ]

    let movementIcon m =
      if m > 0 then
        [ Fa.i [ Fa.CustomClass "movement-up"
                 Fa.Size Fa.FaSmall
                 Fa.Solid.AngleUp ] [] ]
      elif m < 0 then
        [ Fa.i [ Fa.CustomClass "movement-down"
                 Fa.Size Fa.FaSmall
                 Fa.Solid.AngleDown ] [] ]
      else
        [ Fa.i [ Fa.CustomClass "movement-none"
                 Fa.Size Fa.FaExtraSmall
                 Fa.Solid.Minus ] [] ]

    Table.table [ Table.IsHoverable; Table.IsFullWidth ] [
      thead [] [
        tr [] [
          th [] []
          th [] []
          th [] []
          th [ Class CustomClasses.TextRight ] [
            str "CR"
          ]
          th [ Class CustomClasses.TextRight ] [
            str "CS"
          ]
          th [ Class CustomClasses.TextRight ] [
            str "Pts"
          ]
        ]
      ]
      tbody
        []
        (league.Members
         |> List.map
              (fun (pId, m) ->
                let (p, cr, cs) = toShortPoints m.Points

                tr [ ClassName(
                       if activePlayerId = pId then
                         "is-selected"
                       else
                         ""
                     ) ] [
                  td [ Class CustomClasses.TextRight ] [
                    str (string m.Position)
                  ]
                  td [ Class CustomClasses.TextCenter ] (movementIcon m.Movement)
                  td [] [ playerLink pId m ]
                  td [ Class CustomClasses.TextRight ] [
                    str (string cr)
                  ]
                  td [ Class CustomClasses.TextRight ] [
                    str (string cs)
                  ]
                  td [ Class CustomClasses.TextRight ] [
                    str (string p)
                  ]
                ]))
    ]

  let smallIconWithText iconName text =
    div [ Style [ MarginLeft "2px" ] ] [
      Fa.i [ iconName; Fa.Size Fa.FaSmall ] []
      span [ Style [ MarginLeft "2px" ] ] [
        str text
      ]
    ]

  module Social =

    let twitterHref content =
      sprintf "https://twitter.com/intent/tweet?text=%s" content

    let facebookHref content =
      sprintf "https://www.facebook.com/sharer/sharer.php?u=%s" content

    let whatsAppHref content =
      sprintf "whatsapp://send?text=%s" content


  open Shared.Teams

  let badgeAbbrv (Team team) =
    match team with
    | Arsenal -> "ARS"
    | AstonVilla -> "AVL"
    // | Bournemouth -> "BOU"
    | Brentford -> "BRE"
    | Brighton -> "BHA"
    | Burnley -> "BUR"
    // | Cardiff -> "CAR"
    | Chelsea -> "CHE"
    | CrystalPalace -> "CRY"
    | Everton -> "EVE"
    // | Fulham -> "FUL"
    // | Huddersfield -> "HUD"
    | Leeds -> "LEE"
    | Leicester -> "LEI"
    | Liverpool -> "LIV"
    | ManCity -> "MCI"
    | ManUtd -> "MUN"
    | Newcastle -> "NEW"
    | Norwich -> "NOR"
    // | SheffieldUtd -> "SHU"
    | Southampton -> "SOU"
    | Spurs -> "TOT"
    | Watford -> "WAT"
    // | WestBrom -> "WBA"
    | WestHam -> "WHU"
    | Wolves -> "WOL"
    | _ -> ""

  type BadgeSize =
    | S
    | M
    | L
    | XL

  let badgeSizePx =
    function
    | S -> 20
    | M -> 25
    | L -> 50
    | XL -> 100

  let badge size team =
    let clas' =
      sprintf "badge-%i %s" (badgeSizePx size) (badgeAbbrv team)

    Text.span [ Modifiers []; Props [ Class clas' ] ] []

  let shortTeamName team =
    div [ Class "" ] [
      span [ Class "short-team-name" ] [
        str (badgeAbbrv team)
      ]
    ]

  let kickOffTime (ko: KickOff) =
    Text.span [ Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is2) ]
                Props [ Class "kick-off-time" ] ] [
      str (ko.Raw.ToString("HH:mm"))
    ]

  let gameweekDate (KickOffGroup s) = subHeading s

  module ScoreBox =

    let private boxes clas' h a modifier =
      [ div [ Class <| sprintf "scorebox scorebox-left %s" clas' ] [
          str h
        ]
        div [ Class
              <| sprintf "scorebox scorebox-right %s" clas' ] [
          str a
        ] ]

      |> fun homeAndAway ->
           (match modifier with
            | PredictionModifier.None -> []
            | PredictionModifier.BigUp ->
              [ div [ Class "scorebox-bigup" ] [
                  Fa.i [ Fa.Solid.AngleDoubleUp ] []
                ] ]
            | PredictionModifier.DoubleDown ->
              [ div [ Class "scorebox-dd" ] [
                  Fa.i [ Fa.Solid.AngleDoubleDown ] []
                ] ])
           |> fun dd -> div [ Class "scorebox-container" ] (homeAndAway @ dd)

    let emptyScoreBox () =
      boxes "no-points" "_" "_" PredictionModifier.None

    let openScoreBox (ScoreLine (Score h, Score a)) =
      boxes "open" (string h) (string a) PredictionModifier.None

    let kickedOffScoreBox (ScoreLine (Score h, Score a)) modifier =
      boxes "kicked-off" (string h) (string a) modifier

    let classifiedScoreBox (ScoreLine (Score h, Score a)) modifier category =
      match category with
      | CorrectResult -> "correct-result"
      | CorrectScore -> "correct-score"
      | Incorrect -> "no-points"
      |> fun clas' -> boxes clas' (string h) (string a) modifier

    let resultScoreBox (ScoreLine (Score h, Score a)) =
      boxes "result" (string h) (string a) PredictionModifier.None

  open Routes

  let panelAnchor icon text nav route =
    Panel.Block.a [ Panel.Block.Props [ OnClick (fun e -> e.preventDefault(); nav route); (Routes.href route) ] ] [
      Panel.icon [] [
        Fa.i [icon] []
      ]
      str text
    ]

  let panelAnchorExternalUrl icon text url =
    Panel.Block.a [ Panel.Block.Props [ (Href url) ] ] [
      Panel.icon [] [
        Fa.i [icon] []
      ]
      str text
    ]

  let leagueMenu leagueId gwno nav =
    Panel.panel [ Panel.Color IsPrimary ] [
      panelAnchor Fa.Solid.Trophy "Table" nav (LeaguesRoute(LeagueTableRoute leagueId))
      panelAnchor Fa.Solid.History "History" nav (LeaguesRoute(LeagueHistoryRoute leagueId))
      panelAnchor Fa.Solid.Table (sprintf "Gameweek %i Matrix" gwno) nav (LeaguesRoute(LeagueMatrixRoute(leagueId, gwno)))
    ]
