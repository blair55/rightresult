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
      ShowFeedbackModal: bool
      FeedbackText: string
      IsSubscribable: bool
      IsSubscribing: bool }

  type Msg =
    | Init of Result<string, exn>
    | AlertInfo of string
    | PlayerPointsTotalRecieved of Rresult<PredictionPointsMonoid>
    | GlobalGwWinnerReceived of Rresult<GlobalGameweekWinner option>
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

  let heroBar (model: Model) points =
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

  let ``dah dit`` left right =
    Columns.columns [ Columns.IsMobile
                      Columns.Props [ Props.Style [ MarginBottom "1em" ] ] ] [
      Column.column [ Column.Modifiers []
                      Column.Width(Screen.All, Column.IsTwoThirds) ] [
        Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column
                               Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
          div [] left
        ]
      ]
      Column.column [ Column.Modifiers []
                      Column.Width(Screen.All, Column.IsOneThird) ] [
        Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column ]
                   Props [ Style [ Direction DirectionOptions.Rtl ] ] ] [
          div [] right
        ]
      ]
    ]

  let ``dit dah`` left right =
    Columns.columns [ Columns.IsMobile
                      Columns.Props [ Props.Style [ MarginBottom "1em" ] ] ] [
      Column.column [ Column.Modifiers []
                      Column.Width(Screen.All, Column.IsOneThird) ] [
        Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column
                               Modifier.FlexJustifyContent FlexJustifyContent.FlexStart ] ] [
          div [] left
        ]
      ]
      Column.column [ Column.Modifiers []
                    // Column.Width(Screen.All, Column.IsOneThird)
                     ] [
        Text.div [ Modifiers [ Modifier.FlexDirection FlexDirection.Column ]
                   Props [ Style [ Direction DirectionOptions.Rtl ] ] ] [
          div [] right
        ]
      ]
    ]

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

  let homeMenu model dispatch =
    let (PlayerId playerId) = model.Player.Id

    Box.box' [] [

      div [ Class "block" ] [

        Button.button
          ([ Button.IsFullWidth
             Button.Color IsInfo
             Button.IsOutlined
             Button.IsLight
             Button.OnClick(fun _ -> Logout |> dispatch) ])
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

    // div [ Class "block" ] [

    //   Button.button
    //     ([ Button.IsFullWidth
    //        Button.Color IsWarning
    //        Button.IsLight
    //        Button.OnClick(fun _ -> Logout |> dispatch) ])
    //     [ str "preview badges >>" ]
    // ]

    // div [ Class "block" ] [

    //   Button.button
    //     ([ Button.IsFullWidth
    //        Button.IsOutlined
    //        Button.Color IsDanger
    //        Button.OnClick(fun _ -> Logout |> dispatch) ])
    //     [ str "log out >>" ]
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

  let fixtureReel (fixtures: NewFixtureSetViewModel) =
    div [ Class "fixture-reel-container" ] [
      div
        [ Class "fixture-reel" ]
        (Teams.all
         |> List.sortBy (fun _ -> rand.Next())
         |> halveList
         ||> List.zip
         |> List.map TeamLine
         |> List.map fixtureReelItem)
    ]

  let vm =
    { GameweekNo = GameweekNo 1
      Fixtures =
        [ (Ko.create DateTime.Now, KickOffGroup "", TeamLine(Team Teams.Arsenal, Team Teams.AstonVilla))
          (Ko.create DateTime.Now, KickOffGroup "", TeamLine(Team Teams.Brentford, Team Teams.Brighton))
          (Ko.create DateTime.Now, KickOffGroup "", TeamLine(Team Teams.Burnley, Team Teams.Chelsea))
          (Ko.create DateTime.Now, KickOffGroup "", TeamLine(Team Teams.CrystalPalace, Team Teams.Everton)) ] }

  let loadedView (model: Model) (points, winner) dispatch =
    [ heroBar model points
      // notificationPrompt model dispatch
      // gwWinner dispatch winner
      // playerBar dispatch model points
      titleBar
      fixtureReel vm
      homeMenu model dispatch ]

  [<Emit("isSubscribableToPush()")>]
  let isSubscribableToPush () : JS.Promise<bool> = jsNative

  [<Emit("subscribeToPush()")>]
  let subscribeToPush () : JS.Promise<PushSubscription> = jsNative

  let noNetworkView model dispatch =
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
    match model.TotalPoints, model.GlobalGwWinner with
    | Success points, Success winner -> div [ Class "home" ] (loadedView model (points, winner) dispatch)
    | WebError _, _
    | _, WebError _ -> noNetworkView model dispatch
    | _ -> div [] []

  let init api player =
    { Player = player
      TotalPoints = Fetching
      // LeagueTable = Fetching
      GlobalGwWinner = Fetching
      ShowFeedbackModal = false
      FeedbackText = ""
      IsSubscribable = false
      IsSubscribing = false },
    Cmd.batch [ Cmd.OfAsync.perform (api.getPlayerPointsTotal player.Id) player.Token PlayerPointsTotalRecieved
                Cmd.OfAsync.perform api.getGlobalGameweekWinner player.Token GlobalGwWinnerReceived
                Cmd.OfPromise.perform isSubscribableToPush () InitIsSubscribableReceived ]

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
    | Logout _ -> model, []
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
