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

module HomeArea =

  type Model =
    { Player: ClientSafePlayer
      TotalPoints: PredictionPointsMonoid WebData
      GlobalGwWinner: WebData<GlobalGameweekWinner option>
      BigUps: WebData<BigUpViewModel list>
      ShowFeedbackModal: bool
      FeedbackText: string
      IsSubscribable: bool
      IsSubscribing: bool }

  type Msg =
    | Init of Result<string, exn>
    | AlertInfo of string
    | PlayerPointsTotalRecieved of Rresult<PredictionPointsMonoid>
    | GlobalGwWinnerReceived of Rresult<GlobalGameweekWinner option>
    | BigUpsReceived of Rresult<BigUpViewModel list>
    | NavTo of Route
    | Logout
    | ShowFeedbackModal
    | EditFeedback of string
    | HideFeedbackModal
    | SubmitFeedback
    | InitIsSubscribableReceived of bool
    | DismissSubscribePrompt
    | Subscribe
    | Subscribed of PushSubscription

  let button attr txt onClick =
    Button.button ([ Button.OnClick onClick ] @ attr) [ str txt ]

  // let ``dah dit`` left right =
  //   Columns.columns [ Columns.IsMobile
  //                     Columns.Props [ Props.Style [ MarginBottom "1em" ] ] ] [
  //     Column.column [ Column.Modifiers []
  //                     Column.Width(Screen.All, Column.IsTwoThirds) ] [
  //       Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column
  //                              Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
  //         div [] left
  //       ]
  //     ]
  //     Column.column [ Column.Modifiers []
  //                     Column.Width(Screen.All, Column.IsOneThird) ] [
  //       Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column ]
  //                  Props [ Style [ Direction DirectionOptions.Rtl ] ] ] [
  //         div [] right
  //       ]
  //     ]
  //   ]

  // let ``dit dah`` left right =
  //   Columns.columns [ Columns.IsMobile
  //                     Columns.Props [ Props.Style [ MarginBottom "1em" ] ] ] [
  //     Column.column [ Column.Modifiers []
  //                     Column.Width(Screen.All, Column.IsOneThird) ] [
  //       Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column
  //                              Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
  //         div [] left
  //       ]
  //     ]
  //     Column.column [ Column.Modifiers []
  //                   // Column.Width(Screen.All, Column.IsOneThird)
  //                    ] [
  //       Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column ]
  //                  Props [ Style [ Direction DirectionOptions.Rtl ] ] ] [
  //         div [] right
  //       ]
  //     ]
  //   ]

  let lightInfoBox e = Box.box' [] [ e ]

  let titleBar =
    div [] [
      Components.pageTitle "Get your predictions in"
    ]
  //   Components.pageTitle "Points"
  //   div [ Class "tile-box" ] [ lightInfoBox (str (string points.Points)) ]


  let playerBar dispatch { Model.Player = { Id = PlayerId playerId; Name = name } } (points: PredictionPointsMonoid) =
    div [] []
  // div [] [
  //   Components.pageTitle "Welcome"
  //   div [ Class "tile-box" ] [ lightInfoBox (str (string name)) ]
  //   Components.pageTitle "Points"
  //   div [ Class "tile-box" ] [ lightInfoBox (str (string points.Points)) ]

  //   ``dah dit`` [ div [] [
  //                   Components.pageTitle "Welcome"
  //                 ]
  //                 div [ Class "tile-box" ] [
  //                   lightInfoBox (str (string name))
  //                 ] ] [
  //     div [] [ Components.pageTitle "Points" ]
  //     div [ Class "tile-box" ] [
  //       lightInfoBox (str (string points.Points))
  //     ]
  //   ]

  //   div [ Class "fixture-reel" ]
  //     []

  //   ``dit dah`` [ div [] [
  //                   Components.pageTitle "Fixtures"
  //                 ]
  //                 div [ Class "tile-box" ] [
  //                   lightInfoBox (str ("10"))
  //                 ] ] [
  //     div [] [
  //       Components.pageTitle "GW 1 Leader"
  //     ]
  //     div [ Class "tile-box" ] [
  //       lightInfoBox (str ("No one yet"))
  //     ]
  //   ]
  // ]

  let gwWinner dispatch =
    function
    | Some { GameweekNo = GameweekNo gwno
             PlayerId = PlayerId playerId
             Member = { LeagueTableMember.PlayerName = (PlayerName playerName)
                        Points = m } } ->
      Hero.hero [ Hero.Color IsWarning
                  Hero.IsBold
                  Hero.Props [ Style [ MarginBottom "1em" ]
                               OnClick
                                 (fun _ ->
                                   PlayerRoute playerId
                                   |> PlayersRoute
                                   |> NavTo
                                   |> dispatch) ] ] [
        Hero.body [] [
          Container.container [] [
            Heading.h4 [ Heading.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ] ] [
              span [] [
                str <| sprintf "GW %i Winner" gwno
              ]
            ]
            Heading.h6 [ Heading.IsSubtitle ] [
              Fa.i [ Fa.Solid.Medal ] []
              str <| sprintf " %s • %ipts" playerName m.Points
            ]
          ]
        ]
      ]
    | None -> div [] []

  let notificationPrompt model dispatch =
    let subscribeButton (model: Model) dispatch =
      match model.IsSubscribing with
      | false -> button [ Button.Color IsInfo ] "Enable Notifications" (fun _ -> dispatch Subscribe)
      | true ->
        button
          [ Button.Color IsWhite
            Button.IsLoading true ]
          ""
          ignore

    if model.IsSubscribable then
      Components.cardWithFooter [ Message.message [ Message.Color IsInfo ] [
                                    Message.header [] [
                                      str "Notifications"
                                      Delete.delete [ Delete.OnClick(fun _ -> dispatch DismissSubscribePrompt) ] []
                                    ]
                                    Message.body [ Modifiers [ Modifier.TextAlignment(Screen.Mobile, TextAlignment.Left) ] ] [
                                      str "Get push notifications on this device"
                                    ]
                                  ] ] [
        Card.Footer.a [] [
          subscribeButton model dispatch
        ]
      ]
    else
      div [] []

  let homeMenu dispatch =

    Box.box' [] [

      div [ Class "block" ] [

        Button.button
          ([ Button.IsFullWidth
             Button.Color IsInfo
             Button.IsOutlined
             Button.IsLight
             Button.OnClick(fun _ -> NavTo HowItWorksRoute |> dispatch) ])
          [ str "How it works"
            Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
      ]

      div [ Class "block" ] [

        Button.button
          ([ Button.IsFullWidth
             Button.IsOutlined
             Button.Color IsWarning
             Button.IsLight
             Button.OnClick(fun _ -> NavTo(LeaguesRoute(CreateLeagueRoute)) |> dispatch) ])
          [ str "Create a league"
            Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
      ]

      div [ Class "block" ] [

        Button.button
          ([ Button.IsFullWidth
             Button.Color IsDanger
             Button.IsOutlined
             Button.IsLight
             Button.OnClick
               (fun _ ->
                 NavTo(GameweekRoute(GameweekInitRoute))
                 |> dispatch) ])
          [ str "Predict"
            Fa.i [ Fa.Solid.AngleDoubleRight ] [] ]
      ]

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
    div [ Class "is-clearfix"
          Style [ MarginBottom "1em" ] ] [
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
    ]

  let bigUpsBar dispatch bigups =
    let one, two = halveList bigups

    div [ Class "is-clearfix" ] [
      div [ Class "big-up-box-container hide-scrollbars" ] [
        div [ Class "big-up-box-wrapper" ] (List.map (Components.bigUpBox (NavTo >> dispatch)) one)
        div [ Class "big-up-box-wrapper" ] (List.map (Components.bigUpBox (NavTo >> dispatch)) two)
      ]
    ]

  // let cashLeague () =
  //   div [ Class "is-clearfix"
  //         Style [ MarginBottom "1em" ] ] [
  //     Message.message [ Message.Color IsWarning ] [
  //       Message.body [] [
  //         p [] [
  //           a [ Href Routes.contactPath ] [
  //             str "Get in touch to play for cash!"
  //           ]
  //         ]
  //       ]
  //     ]
  //   ]

  let globalLeaderBoard () =
    div []
      [
        Components.subHeading "Gameweek 1"
        div [ Class "global-leaderboard hide-scrollbars" ] [
          // div [ Class "global-leaderboard-head" ] [
          //   str "Global League Leaders GW1"
          // ]
          div [ Class "global-leaderboard-body" ] [
            div [ Class "global-leaderboard-item" ] [
              span [Class"global-leaderboard-pos"] [str "1st"]
              span [Class"global-leaderboard-points"] [str "21 pts"]
              str "@tester"
            ]
            div [ Class "global-leaderboard-item" ] [
              span [Class"global-leaderboard-pos"] [str "2nd"]
              span [Class"global-leaderboard-points"] [str "18 pts"]
              str "@Matthew Jones"
            ]
            div [ Class "global-leaderboard-item" ] [
              span [Class"global-leaderboard-pos"] [str "3rd"]
              span [Class"global-leaderboard-points"] [str "18 pts"]
              str "@Andy Spud Robertson"
            ]
          ]]
      ]

  let loadedView dispatch (model: Model) (points, winner, bigups) =
    [ Components.heroBar
      // notificationPrompt model dispatch
      // gwWinner dispatch winner
      // playerBar dispatch model points
      titleBar
      fixtureReel ()
      bigUpsBar dispatch bigups
      // globalLeaderBoard ()
      // cashLeague ()
      homeMenu dispatch ]

  [<Emit("isSubscribableToPush()")>]
  let isSubscribableToPush () : JS.Promise<bool> = jsNative

  [<Emit("subscribeToPush()")>]
  let subscribeToPush () : JS.Promise<PushSubscription> = jsNative

  let noNetworkView dispatch model =
    div [] [
      Components.card [ Message.message [ Message.Color IsWarning ] [
                          Message.body [ Modifiers [ Modifier.TextAlignment(Screen.Mobile, TextAlignment.Left) ] ] [
                            str "No network!"
                          ]
                        ] ]
      Components.card [ Menu.menu [] [
                          Menu.list [] [
                            // Menu.Item.li [ Menu.Item.OnClick (fun _ -> LeaguesRoute GlobalLeagueRoute |> NavTo |> dispatch) ] [ str "Global League" ]
                            // Menu.Item.li [ Menu.Item.OnClick (fun _ -> PlayerRoute playerId |> PlayersRoute |> NavTo |> dispatch) ] [ str model.Player.Name ]
                            Menu.Item.li [ Menu.Item.OnClick(fun _ -> Logout |> dispatch) ] [
                              str "Log out"
                            ]
                          ]
                        ] ]
    ]

  let view model dispatch =
    match model.TotalPoints, model.GlobalGwWinner, model.BigUps with
    | Success points, Success winner, Success bigups ->
      div [ Class "home" ] (loadedView dispatch model (points, winner, bigups))
    | WebError _, _, _
    | _, WebError _, _ -> noNetworkView dispatch model
    | _ -> div [] []

  let init api player =
    { Player = player
      TotalPoints = Fetching
      // LeagueTable = Fetching
      GlobalGwWinner = Fetching
      BigUps = Fetching
      ShowFeedbackModal = false
      FeedbackText = ""
      IsSubscribable = false
      IsSubscribing = false },
    Cmd.batch [ Cmd.OfAsync.perform (api.getPlayerPointsTotal player.Id) player.Token PlayerPointsTotalRecieved
                Cmd.OfAsync.perform api.getGlobalGameweekWinner player.Token GlobalGwWinnerReceived
                Cmd.OfPromise.perform isSubscribableToPush () InitIsSubscribableReceived
                Cmd.OfAsync.perform api.getHomePageBigUps player.Token BigUpsReceived ]

  let safeDateTimeToString (s: string) =
    match DateTime.TryParse s with
    | true, d -> d
    | _ -> DateTime.MinValue

  let isOver60Days (d: DateTime) = d.Date.AddDays 60. < DateTime.Now.Date

  let dismissSubscriptionStorageKey = "dismiss-subscription"

  let update (api: IProtocol) player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | PlayerPointsTotalRecieved r ->
      { model with
          TotalPoints = resultToWebData r },
      []
    // | LeagueTableReceived r -> { model with LeagueTable = resultToWebData r }, []
    | GlobalGwWinnerReceived r ->
      { model with
          GlobalGwWinner = resultToWebData r },
      []
    | BigUpsReceived r ->
      { model with
          BigUps = resultToWebData r },
      []
    | ShowFeedbackModal -> { model with ShowFeedbackModal = true }, []
    | HideFeedbackModal -> { model with ShowFeedbackModal = false }, []
    | EditFeedback s -> { model with FeedbackText = s }, []
    | SubmitFeedback ->
      { model with
          ShowFeedbackModal = false
          FeedbackText = "" },
      Cmd.OfAsync.perform
        (api.submitFeedback model.FeedbackText)
        player.Token
        (fun _ -> AlertInfo "Thanks for your feedback!")
    | NavTo r -> model, navTo r
    | AlertInfo s ->
      model,
      (Toast.message
       >> Toast.position Toast.TopCenter
       >> Toast.info)
        s
    | Logout -> model, []
    | InitIsSubscribableReceived isSubscribable ->
      localStorage.getItem dismissSubscriptionStorageKey
      |> safeDateTimeToString
      |> isOver60Days
      |> fun isDismissExpired ->
           { model with
               IsSubscribable = isSubscribable && isDismissExpired },
           []
    | DismissSubscribePrompt ->
      localStorage.setItem (dismissSubscriptionStorageKey, DateTime.Now.Date.ToString("yyyy-MM-dd"))
      { model with IsSubscribable = false }, []
    | Subscribe -> { model with IsSubscribing = true }, Cmd.OfPromise.perform subscribeToPush () Subscribed
    | Subscribed sub ->
      localStorage.removeItem dismissSubscriptionStorageKey

      { model with
          IsSubscribing = false
          IsSubscribable = false },
      Cmd.OfAsync.perform (api.subscribeToPush player.Token) sub (fun _ -> AlertInfo "Notifications enabled")
