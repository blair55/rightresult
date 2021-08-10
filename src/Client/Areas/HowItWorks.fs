namespace Areas

open Elmish.React

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared
open Fulma
open Components

module HowItWorksArea =

  let button attr txt onClick =
    Button.button ([ Button.OnClick onClick ] @ attr) [ str txt ]

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
          str "Irrespective of the result, a prediction will be awarded bonus points for any of the following:"
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
          str "A prediction with the correct scoreline will be awarded all bonuses and return the maximum "
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
            The points awarded will be doubled.
            You can change your mind about your Double Down any time before kick off."
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
          str "A Big Up prediction with a correct scoreline will be awarded an additional "
          b [] [ str "3 points" ]
          str ". "
          str "A Big Up prediction with a correct result will be awarded an additional "
          b [] [ str "1 point" ]
          str "."
        ]
      ]
    ]

  let homeMenu =
    Box.box' [] [
      div [ Class "block" ] [
        a [ Href Routes.gwPath
            Class "button is-light is-outlined is-info is-fullwidth" ] [
          span [ Style [ MarginRight "3px" ] ] [
            str "Get started"
          ]
          Fa.i [ Fa.Solid.AngleDoubleRight ] []
        ]
      ]
      div [ Class "block" ] [
        a [ Href Routes.contactPath
            Class "button is-light is-outlined is-info is-fullwidth" ] [
          span [ Style [ MarginRight "3px" ] ] [
            str "Get in touch"
          ]
          Fa.i [ Fa.Solid.AngleDoubleRight ] []
        ]
      ]
    ]

  let view _ =
    div [ Class "how-it-works" ] [
      Components.heroBar
      titleBar
      winningBar
      pointsBar
      doubleDownBar
      bigUpBar
      homeMenu
    ]
