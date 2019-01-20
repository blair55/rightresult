namespace Areas

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma.FontAwesome

open Shared
open Fulma

module CustomClasses =

  let [<Literal>] TextRight = "text-right"
  let [<Literal>] TextLeft = "text-left"

module Components =

  let pointsTotalView (total:PredictionPointsMonoid) =
    let brokenSpan one two =
      [ span [] [ str one ]
        br []
        span [] [ str two ]
      ]
    Level.level [ Level.Level.Option.IsMobile ]
      [ Level.item [ Level.Item.HasTextCentered ]
          [ div [ ]
              [ Level.heading [ ] (brokenSpan "correct" "results")
                Level.title [ ] [ str (string (total.CorrectResults + total.DoubleDownCorrectResults)) ]
              ]
          ]
        Level.item [ Level.Item.HasTextCentered ]
          [ div [ ]
              [ Level.heading [ ] (brokenSpan "correct" "scores")
                Level.title [ ] [ str (string (total.CorrectScores + total.DoubleDownCorrectScores)) ]
              ]
          ]
        Level.item [ Level.Item.HasTextCentered ]
          [ div [ ]
              [ Level.heading [ ] (brokenSpan "total" "points")
                Level.title [ ] [ str (string total.Points) ]
              ]
          ]
      ]

  let pageTitle s =
    div [ Style [ MarginLeft "1em"; MarginTop "1em"; MarginBottom "1em" ] ]
      [ Heading.h3 [] [ str s ] ]

  let subHeading s =
    Heading.h5 [ Heading.IsSubtitle; Heading.Props [ Style [ MarginLeft "1em" ] ] ] [ str s ]

  let teamName (Team team) =
    Text.span
      [ Modifiers [ Modifier.TextSize (Screen.Mobile, TextSize.Is7) ] ]
      [ str team ]

  let teamNameNotOnMobile (Team team) =
    Text.span
      [ Modifiers [ Modifier.IsHidden (Screen.Mobile, true) ] ]
      [ str team ]

  let pluralPoints p =
    if p = 1 then "pt" else "pts"
    |> sprintf "%i %s" p
    |> str

  let card content =
    div [ Style [ MarginBottom "1em" ] ]
      [ Card.card [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
          [ Card.content [] content
          ]
      ]

  let cardWithFooter content footer =
    div [ Style [ MarginBottom "1em" ] ]
      [ Card.card [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
          [ Card.content [] content
            Card.footer []
              footer
          ]
      ]

  let toShortPoints (m:PredictionPointsMonoid) =
    m.Points, m.CorrectResults + m.DoubleDownCorrectResults, m.CorrectScores + m.DoubleDownCorrectScores

  let table (league:LeagueTableDoc) (playerClick:PlayerId -> Unit) =
    let playerLink pId (m:LeagueTableMember) =
      let (PlayerName name) = m.PlayerName
      a [ OnClick (fun _ -> playerClick pId) ] [ str name ]
    Table.table [ Table.IsHoverable; Table.IsFullWidth ]
      [ thead []
          [ tr []
              [ th [] []
                th [] []
                th [ Class CustomClasses.TextRight ] [ str "CR" ]
                th [ Class CustomClasses.TextRight ] [ str "CS" ]
                th [ Class CustomClasses.TextRight ] [ str "Pts" ] ] ]
        tbody [] (
          league.Members
          |> List.map (fun (pId, m) ->
            let (p, cr, cs) = toShortPoints m.Points
            tr []
               [ td [ Class CustomClasses.TextRight ] [ str (string m.Position) ]
                 td [] [ playerLink pId m ]
                 td [ Class CustomClasses.TextRight ] [ str (string cr) ]
                 td [ Class CustomClasses.TextRight ] [ str (string cs) ]
                 td [ Class CustomClasses.TextRight ] [ str (string p) ]
               ]
          ))
      ]

  let icon i =
    Icon.faIcon [] [ Fa.icon i ]

  open Shared.Teams

  let badgeAbbrv (Team team) =
    match team with
    | Arsenal -> "ARS"
    | Bournemouth -> "BOU"
    | Brighton -> "BHA"
    | Burnley -> "BUR"
    | Cardiff -> "CAR"
    | Chelsea -> "CHE"
    | CrystalPalace -> "CRY"
    | Everton -> "EVE"
    | Fulham -> "FUL"
    | Huddersfield -> "HUD"
    | Leicester -> "LEI"
    | Liverpool -> "LIV"
    | ManCity -> "MCI"
    | ManUtd -> "MUN"
    | Newcastle -> "NEW"
    | Southampton -> "SOU"
    | Spurs -> "TOT"
    | Watford -> "WAT"
    | WestHam -> "WHU"
    | Wolves -> "WOL"
    | _ -> ""

  type BadgeSize =
    | S
    | M
    | L
    | XL

  let badgeSizePx = function
    | S -> 20
    | M -> 25
    | L -> 50
    | XL -> 100

  let badge size team =
    let clas' = sprintf "badge-%i %s" (badgeSizePx size) (badgeAbbrv team)
    Text.span [ Modifiers [ ]; Props [ Class clas' ] ] []

  module ScoreBox =

    let private boxes clas' h a dd =
      [ div [ Class <| sprintf "scorebox scorebox-left %s" clas' ]
          [ str h ]
        div [ Class <| sprintf "scorebox scorebox-right %s" clas' ]
          [ str a ]
      ]
      |> fun homeAndAway ->
        if dd then
          [ div [ Class "scorebox-dd" ] [ icon Fa.I.AngleDoubleDown ] ]
        else
          []
      |> fun dd ->
        div [ Class "scorebox-container" ] (homeAndAway @ dd)

    let emptyScoreBox () =
      boxes "no-points" "_" "_" false

    let openScoreBox (ScoreLine (Score h, Score a)) =
      boxes "open" (string h) (string a) false

    let kickedOffScoreBox (ScoreLine (Score h, Score a)) dd =
      boxes "kicked-off" (string h) (string a) dd

    let classifiedScoreBox (ScoreLine (Score h, Score a)) dd category =
      match category with
      | CorrectResult -> "correct-result"
      | CorrectScore -> "correct-score"
      | Incorrect -> "no-points"
      |> fun clas'->
      boxes clas' (string h) (string a) dd

    let resultScoreBox (ScoreLine (Score h, Score a)) =
      boxes "result" (string h) (string a) false
