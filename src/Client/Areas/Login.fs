namespace Areas

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes

module LoginArea =

  let getQs key =
    match getQueryStringValue key with
    | Some v -> v
    | _ -> ""

  let view _ =
    div []
      [ Components.pageTitle "Login"
        Card.card []
          [ Card.content []
              [ p [] [ str "You are logged out" ]
              ]
            Card.footer []
              [ Card.Footer.a []
                  [ form [ HTMLAttr.Method "POST"; Action Routes.redirectToFacebookPath ]
                      [
                        input [ Type "hidden"; Name redirectPathKey; Value (getQs redirectPathKey)]
                        Button.button
                          [ Button.IsFullWidth
                            Button.Props [ Type "submit" ]
                            Button.IsLink ]
                          // [ Icon.faIcon [ Icon.CustomClass "fab" ] [ Fable.Helpers.React.i [] [] ] //[ Fa.icon Fa.I.FacebookSquare; Fa.faLg ]
                            [ span [ Class "icon" ] [ i [ Class "fab fa-facebook-square fa-lg" ] [] ]
                              span [] [ str "Login" ]
                            ]
                      ]
                  ]
                Card.Footer.a []
                  [ form [ HTMLAttr.Method "POST"; Action Routes.redirectToTwitterPath ]
                      [
                        input [ Type "hidden"; Name redirectPathKey; Value (getQs redirectPathKey)]
                        Button.button
                          [ Button.IsExpanded
                            Button.Props [ Type "submit" ]
                            Button.IsLink ]
                          // [ Icon.faIcon [] [ Fa.icon Fa.I.TwitterSquare; Fa.faLg ]
                          //   span [] [ str "Login" ]
                          // ]
                          [ span [ Class "icon" ] [ i [ Class "fab fa-twitter-square fa-lg" ] [] ]
                            span [] [ str "Login" ]
                          ]
                      ]
                  ]
              ]
          ]
      ]