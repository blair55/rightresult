namespace Areas

open Elmish
open Elmish.React
open Thoth.Elmish

open Fable.Core
open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared
open Fulma
open Routes
open System
open Browser.WebStorage

module HowItWorksArea =

  type Model = Unit

  type Msg =
    | Init of Result<string, exn>
    | NavTo of Route

  let button attr txt onClick =
    Button.button ([ Button.OnClick onClick ] @ attr) [ str txt ]

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

  let lightInfoBox e = Box.box' [] [ e ]

  let titleBar =
    div [] [
      Components.pageTitle "How it works"
      Box.box' [] [
        p [] [
          str
            "Right Result is fantasy football for match predictions.
            Predict on every premier league fixture of the season and prove how much you know!"
        ]
      ]
    ]

  let winningBar =
    div [ Class "small-text" ] [
      Components.subHeading "Winning"
      Box.box' [] [
        p [] [
          str
            "Predict once per fixture. Points awarded will apply to the Global League and every private league you belong to."
        ]
        p [] [
          str
            "Private leagues are invite-only so share the link to play against your mates.
            Review winners by every gameweek and month throughout the season.
            Create and join as many private leagues as you like."
        ]
      ]
    ]

  let pointsBar =
    div [ Class "small-text" ] [
      Components.subHeading "Points Scoring"
      Box.box' [] [
        p [] [
          str "A prediction with the correct result (home/away win or draw) will be awarded "
          b [] [ str "2 points" ]
          str "."
        ]
        p [] [
          str "Irrespective of the result, a prediction can be awarded bonus points for any of the following:"
        ]
        p [] [
          li [] [
            b [] [ str "1 point" ]
            str " for the correct home team score"
          ]
          li [] [
            b [] [ str "1 point" ]
            str " for the correct away team score"
          ]
          li [] [
            b [] [ str "1 point" ]
            str " for the correct goal difference"
          ]
        ]
        p [] [
          str "The maximum award with all bonuses is "
          b [] [ str "5 points" ]
          str ". "
          str "A prediction that gets the result incorrect may still be awarded a bonus point."
        ]
      ]
    ]

  let doubleDownBar =
    div [ Class "small-text" ] [
      Components.subHeading "Double Down"
      Box.box' [] [
        p [] [
          str
            "The Double Down option can be applied to one fixture per gameweek.
            Any points awarded will be doubled. You can change your mind about your double down
            any time before your selected double down fixture kicks off."
        ]
        p [] [
            str "The maxium return for using Double Down is "
            b [] [ str "10 points" ]
            str "."
            str " The Double Down option cannot be combined with the Big Up option."
        ]
      ]
    ]

  let bigUpBar =
    div [ Class "small-text" ] [
      Components.subHeading "Big Up"
      Box.box' [] [
        p [] [
          str
            "The Big Up option can be applied to one fixture per gameweek.
            Big Up predictions are visible to all players before kick off.
            The Big Up option must be applied over one hour before kick off."
        ]
        p [] [
          str
            "You cannot change your mind about using the Big Up option after it has been applied.
            You cannot edit the scoreline of a Big Up prediction or combine with the Double Down option."
        ]
        p [] [
            str "A Big Up prediction with a correct score is awarded an additional "
            b [] [ str "3 points" ]
            str ". "
            str "A Big Up prediction with a correct result is awarded an additional "
            b [] [ str "1 point" ]
            str "."
        ]
      ]
    ]

  let homeMenu =

    Box.box' [] [

      div [ Class "block" ] [
        a [ Href "/gameweek"
            Class "button is-light is-outlined is-info is-fullwidth" ]
          [ span [ Style [ MarginRight "3px" ] ] [
              str "Get started"
            ]
            Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]

        // Button.a
        //   ([ Button.IsFullWidth
        //      Button.Color IsInfo
        //      Button.IsOutlined
        //      Button.IsLight
        //     //  Button.Modifiers [ IModifier. ]
        //       //  (fun _ ->
        //       //    NavTo(GameweekRoute(GameweekInitRoute))
        //       //    |> dispatch) ]
        //   ]
        //          )
        //   [ span [ Style [ MarginRight "3px" ] ] [
        //       str "Get started"
        //     ]
        //     Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
      ]

    // div [ Class "block" ] [

    //   Button.button
    //     ([ Button.IsFullWidth
    //        Button.IsOutlined
    //        Button.Color IsWarning
    //        Button.IsLight
    //        Button.OnClick(fun _ -> NavTo(LeaguesRoute(CreateLeagueRoute)) |> dispatch) ])
    //     [ str "Create a league"
    //       Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
    // ]

    // div [ Class "block" ] [

    //   Button.button
    //     ([ Button.IsFullWidth
    //        Button.Color IsDanger
    //        Button.IsOutlined
    //        Button.IsLight
    //        Button.OnClick
    //          (fun _ ->
    //            NavTo(GameweekRoute(GameweekInitRoute))
    //            |> dispatch) ])
    //     [ str "Predict"
    //       Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
    // ]

    ]

  let fixtureReelItem (TeamLine (h, a)) =
    div [ Class "fixture-reel-item" ] [
      div [] [
        Components.badge Components.BadgeSize.L h
      ]
      div [] [
        Components.badge Components.BadgeSize.L a
      ]
    ]

  let halveList l = List.splitAt (List.length l / 2) l
  let rand = new Random()

  let fixtureReel () =
    div [ Class "fixture-reel-container hide-scrollbars" ] [
      div
        [ Class "fixture-reel" ]
        (Teams.all
         |> List.sortBy (fun _ -> rand.Next())
         |> halveList
         ||> List.zip
         |> List.map TeamLine
         |> List.map fixtureReelItem)
    ]


  let view _ =
    div [ Class "how-it-works" ] [
      heroBar
      titleBar
      winningBar
      pointsBar
      doubleDownBar
      bigUpBar
      // fixtureReel ()
      homeMenu
    ]

  let init = (), Cmd.none

  let update (api: IProtocol) player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | NavTo r -> model, navTo r
