namespace Areas

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open Fable.FontAwesome

module LoginArea =

  let getQs key =
    match getQueryStringValue key with
    | Some v -> v
    | _ -> ""

  let view _ =
    div [] [
      Components.pageTitle "Login"

      Box.box' [] [
        div [ Class "block" ] [
          str "Login to continue. "
          str "Wait, "
          a [ Style [ TextDecoration "underline" ]
              Routes.href HowItWorksRoute ] [
            str "what is this?"
          ]
        ]
        div [ Class "block" ] [
          form [ HTMLAttr.Method "POST"
                 Action Routes.redirectToFacebookPath ] [
            input [ Type "hidden"
                    Name redirectPathKey
                    Value(getQs redirectPathKey) ]
            Button.button [ Button.IsFullWidth
                            Button.Color Color.IsInfo
                            Button.IsOutlined
                            Button.IsLight
                            Button.Props [ Type "submit" ]
                            Button.IsLink ] [
              Fa.i [ Fa.Size Fa.ISize.FaLarge
                     Fa.Brand.FacebookSquare ] []
              span [ Style [ MarginLeft "5px" ] ] [
                str "Login with facebook"
              ]
            ]
          ]
        ]
        div [ Class "block" ] [
          form [ HTMLAttr.Method "POST"
                 Action Routes.redirectToTwitterPath ] [
            input [ Type "hidden"
                    Name redirectPathKey
                    Value(getQs redirectPathKey) ]
            Button.button [ Button.IsFullWidth
                            Button.Color IsInfo
                            Button.IsOutlined
                            Button.IsLight
                            Button.Props [ Type "submit" ]
                            Button.IsLink ] [
              Fa.i [ Fa.Size Fa.ISize.FaLarge
                     Fa.Brand.TwitterSquare ] []
              span [ Style [ MarginLeft "5px" ] ] [
                str "Login with twitter"
              ]
            ]
          ]
        ]
      ]
    ]
