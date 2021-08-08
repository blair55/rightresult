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

module ContactArea =

  let button attr txt onClick =
    Button.button ([ Button.OnClick onClick ] @ attr) [ str txt ]

  let rand = new Random()

  let rdmBadgeBox () =
    let team = Teams.all.[rand.Next(0, 20)]

    div [ Class "bg-badge-box" ] [
      Components.badge Components.BadgeSize.XL team
    ]

  let titleBar =
    div [] [
      Components.pageTitle "Get In touch"
      Box.box' [] [
        rdmBadgeBox ()
        p [ Class "block" ] [
          str "Comments, questions & ideas are all welcome. We'd love to hear from you!"
        ]
        p [ Class "block" ] [
          a [ Href(Components.Social.twitterHref "@rightresu_lt") ] [
            Fa.i [ Fa.Brand.Twitter ] []
            str "twitter"
          ]
        ]
        p [ Class "block" ] [
          a [ Href("https://www.facebook.com/rightresultrightresult") ] [
            Fa.i [ Fa.Brand.FacebookSquare ] []
            str "facebook"
          ]
        ]
        p [ Class "block" ] [
          a [ Href("mailto:predictionleague1@hotmail.com") ] [
            Fa.i [ Fa.Solid.At ] []
            str "Email"
          ]
        ]
      ]
    ]

  let homeMenu =
    Box.box' [] [
      div [ Class "block" ] [
        a [ Href "/"
            Class "button is-light is-outlined is-info is-fullwidth" ] [
          span [ Style [ MarginRight "3px" ] ] [
            str "Home"
          ]
          Fa.i [ Fa.Solid.AngleDoubleRight ] []
        ]
      ]
      div [ Class "block" ] [
        a [ Href Routes.howItWorksPath
            Class "button is-light is-outlined is-info is-fullwidth" ] [
          span [ Style [ MarginRight "3px" ] ] [
            str "How it works"
          ]
          Fa.i [ Fa.Solid.AngleDoubleRight ] []
        ]
      ]
    ]

  let view _ =
    div [ Class "how-it-works" ] [
      Components.heroBar
      titleBar
      homeMenu
    ]
