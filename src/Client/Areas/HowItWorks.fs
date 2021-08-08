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

  let rand = new Random()
  let rdmBadgeBox () =
    let team = Teams.all.[rand.Next(0, 20)]
    div [Class "bg-badge-box" ][
      Components.badge Components.BadgeSize.XL team
    ]

  let titleBar =
    div [] [
      Components.pageTitle "How it works"
      Box.box' [] [
        rdmBadgeBox ()
        p [] [
          str
            "Right Result is fantasy football for match predictions.
            Predict on every Premier League fixture of the season and prove how much you know!"
        ]
      ]
    ]

  let winningBar =
    div [ Class "small-text" ] [
      Components.subHeading "Winning"
      Box.box' [] [
        rdmBadgeBox ()
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
        rdmBadgeBox ()
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
            str " for the correct home score"
          ]
          li [] [
            b [] [ str "1 point" ]
            str " for the correct away score"
          ]
          li [] [
            b [] [ str "1 point" ]
            str " for the correct goal difference"
            br []
            str " e.g. predict 3-1 when the result is 2-0"
          ]
        ]
        p [] [
          str "A prediction with the correct scoreline is awarded all bonuses and returns the maximum score of "
          b [] [ str "5 points" ]
          str ". "
        ]
        p [] [
          str "A prediction with an incorrect result may still be awarded a bonus point."
        ]
      ]
    ]

  let doubleDownBar =
    div [ Class "small-text" ] [
      Components.subHeading "Double Down"
      Box.box' [] [
        rdmBadgeBox ()
        p [] [
          str
            "The Double Down option can be applied to one fixture per gameweek.
            The points awarded will be doubled. You can change your mind about your double down
            any time before your selected double down fixture kicks off."
        ]
        p [] [
            str "The maximum return for using Double Down is "
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
        rdmBadgeBox ()
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
      ]
    ]

  let view _ =
    div [ Class "how-it-works" ] [
      heroBar
      titleBar
      winningBar
      pointsBar
      doubleDownBar
      bigUpBar
      homeMenu
    ]

  // let init = (), Cmd.none

  // let update (api: IProtocol) player msg model : Model * Cmd<Msg> =
  //   match msg with
  //   | Init _ -> model, []
    // | NavTo r -> model, navTo r
