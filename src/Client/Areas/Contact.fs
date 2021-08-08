namespace Areas

open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared
open Fulma
open Components

module ContactArea =

  let button attr txt onClick =
    Button.button ([ Button.OnClick onClick ] @ attr) [ str txt ]

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
