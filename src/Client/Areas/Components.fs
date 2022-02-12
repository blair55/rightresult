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

  [<Literal>]
  let IsClipped = "is-clipped"

module Html =

  let private elem () =
    Browser
      .Dom
      .document
      .getElementsByTagName("html")
      .Item 0

  let clip =
    elem
    >> fun e -> e.classList.add (CustomClasses.IsClipped)

  let unClip =
    elem
    >> fun e -> e.classList.remove (CustomClasses.IsClipped)

  let elemById elemId =
    Browser.Dom.document.getElementById elemId

  let tryElemById elemId =
    match elemById elemId with
    | null -> None
    | e -> Some e

  let resetScrollTop = elemById >> fun e -> e.scrollTop <- 0.

  let resetScrollToBottom =
    elemById >> fun e -> e.scrollTop <- e.scrollHeight

  let resetScrollLeft = elemById >> fun e -> e.scrollLeft <- 0.

module Components =

  let signedInt i =
    if i > 0 then
      sprintf "%+i" i
    else
      string i

  let simpleScore (ScoreLine (Score h, Score a)) = str (sprintf "%i-%i" h a)

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

  let smallIconWithText iconName text =
    div [ Style [ MarginLeft "2px" ] ] [
      Fa.i [ iconName; Fa.Size Fa.FaSmall ] []
      span [ Style [ MarginLeft "2px" ] ] [
        str text
      ]
    ]

  let leagueIdStr =
    function
    | GlobalLeague -> Global.identifier
    | PrivateLeague (PrivateLeagueId id) -> string id

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

  let rand = new System.Random()

  let bigBackgroundBadge team =
    div [ Class "bg-badge-box" ] [
      badge BadgeSize.XL team
    ]

  let rdmBadgeBox () =
    Teams.all.[rand.Next(0, 20)] |> bigBackgroundBadge

  let shortTeamName team =
    div [ Class "" ] [
      span [ Class "short-team-name" ] [
        str (badgeAbbrv team)
      ]
    ]

  open Routes

  let anchorNavProps nav route =
    [ OnClick
        (fun e ->
          e.preventDefault ()
          nav route)
      :> IHTMLProp
      Routes.href route :> IHTMLProp ]

  let panelAnchor icon text nav route =
    Panel.Block.a [ Panel.Block.Props(anchorNavProps nav route) ] [
      Panel.icon [] [ Fa.i [ icon ] [] ]
      str text
    ]

  let panelAnchorExternalUrl icon text url =
    Panel.Block.a [ Panel.Block.Props [ (Href url) ] ] [
      Panel.icon [] [ Fa.i [ icon ] [] ]
      str text
    ]

  let leagueMenu leagueId gwno nav =
    Panel.panel [ Panel.Color IsPrimary ] [
      panelAnchor Fa.Solid.History "History" nav (LeaguesRoute(LeagueHistoryRoute leagueId))
      panelAnchor Fa.Solid.ThList "Current Table" nav (LeaguesRoute(LeagueTableRoute leagueId))
      panelAnchor
        Fa.Solid.Th
        (sprintf "Gameweek %i Matrix" gwno)
        nav
        (LeaguesRoute(LeagueMatrixRoute(leagueId, gwno)))
    ]

  let bigUpBox
    dispatch
    { PlayerName = PlayerName player
      PlayerId = PlayerId playerId
      ScoreLine = sl
      TeamLine = TeamLine (Team homeTeam, Team awayTeam) }
    =
    div [ Class "big-up-box-item" ] [
      div [ Class "big-up-box-top" ] [
        span [ Class "big-up-box-heading" ] [
          Fa.i [ Fa.Solid.AngleDoubleUp ] []
          str "Big up"
        ]
        a
          ([ Class "big-up-box-player" ]
           @ (anchorNavProps dispatch (PlayersRoute(PlayerRoute playerId))))
          [ str ("@" + player) ]
      ]
      div [ Class "big-up-box-bottom" ] [
        span [ Class "big-up-box-team" ] [
          str homeTeam
        ]
        div [ Class "big-up-box-pred" ] [
          span [] [ simpleScore sl ]
        ]
        span [ Class "big-up-box-team" ] [
          str awayTeam
        ]
      ]
    ]

  let toShortPoints (m: PredictionPointsMonoid) =
    m.Points, m.CorrectResults, m.CorrectScores

  let table nav (league: LeagueTableDoc) activePlayerId =
    let playerLink (PlayerId pId) (m: LeagueTableMember) =
      let (PlayerName name) = m.PlayerName
      a (anchorNavProps nav (PlayerRoute pId |> PlayersRoute)) [ str name ]

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

  let pageGwButtonRow navTo (prev, next) =
    let button icon =
      function
      | Some gwno ->
        Button.button [ Button.Color IsLight
                        Button.Props [ OnClick(fun _ -> navTo gwno) ] ] [
          Fa.i [ icon ] []
        ]
      | None ->
        Button.button [ Button.Disabled true ] [
          Fa.i [ icon ] []
        ]

    Box.box' [] [
      button Fa.Solid.AngleDoubleLeft prev
      button Fa.Solid.AngleDoubleRight next
    ]

  let predictionModifierClass =
    Option.map (fun (_: ScoreLine, modifier) -> PredictionModifier.isModified modifier)
    >> Option.defaultValue false
    >> function
      | true -> "gw-item-ismodified"
      | _ -> ""
