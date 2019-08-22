namespace Areas

open Elmish
open Elmish.React
open Thoth.Elmish

open Fable.Core
open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open System
open Browser.WebStorage

module HomeArea =

  type Model =
    { Player : ClientSafePlayer
      TotalPoints : PredictionPointsMonoid WebData
      LeagueTable : LeagueTableDoc WebData
      ShowFeedbackModal : bool
      FeedbackText : string
      IsSubscribable : bool
      IsSubscribing : bool
    }

  type Msg =
    | Init of Result<string, exn>
    | AlertInfo of string
    | PlayerPointsTotalRecieved of Rresult<PredictionPointsMonoid>
    | LeagueTableReceived of Rresult<LeagueTableDoc>
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
    Button.button
      ([ Button.OnClick onClick ] @ attr)
      [ str txt ]

  let globalLeaguePositionView (model:Model) (league:LeagueTableDoc) dispatch =
    let (_, membr) =
      league.Members |> List.filter (fun (pId, _) -> pId = model.Player.Id) |> List.head
    div []
      [ str <| sprintf "Position %i" membr.Position
      ]

  let loadedView model (league, points) dispatch =
    [ Components.cardWithFooter
        [ globalLeaguePositionView model league dispatch
        ]
        [ Card.Footer.a [ Props [ OnClick (fun _ -> LeaguesRoute GlobalLeagueRoute |> NavTo |> dispatch) ] ]
            [ str "Global League" ]
        ]
      Components.card
        [ Components.pointsTotalView points
        ]
    ]


  let subscribeButton (model:Model) dispatch =
    match model.IsSubscribing with
    | false -> button [Button.Color IsInfo] "Enable Notifications" (fun _ -> dispatch Subscribe)
    | true  -> button [Button.Color IsWhite; Button.IsLoading true] "" ignore

  let feedbackModal dispatch (model:Model) =
    Modal.modal [ Modal.IsActive model.ShowFeedbackModal ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch HideFeedbackModal) ] ]
          []
        Modal.content []
          [ Box.box' []
              [ Heading.h5 [ Heading.IsSubtitle ]
                  [ str "Feedback" ]
                Textarea.textarea
                  [ Textarea.Props [ Style [ MarginBottom "1em" ] ]
                    Textarea.OnChange (fun e -> e.Value |> EditFeedback |> dispatch)
                    Textarea.Placeholder "Comments, suggestions etc"
                    Textarea.Value model.FeedbackText
                  ] []
                Button.button
                  [ Button.Color IsPrimary
                    Button.OnClick (fun _ -> SubmitFeedback |> dispatch)
                  ]
                  [ str "Submit" ]
              ]
          ]
        Modal.close [ Modal.Close.Size IsLarge; Modal.Close.OnClick (fun _ -> dispatch HideFeedbackModal) ]
          []
      ]

  [<Emit("isSubscribableToPush()")>]
  let isSubscribableToPush () : JS.Promise<bool> =
    jsNative

  [<Emit("subscribeToPush()")>]
  let subscribeToPush () : JS.Promise<PushSubscription> =
    jsNative

  let view model dispatch =
    div []
      [ Components.pageTitle "Right Result"

        Components.card
          [ div []
              [ Text.p [ Modifiers [ Modifier.TextWeight TextWeight.SemiBold ] ]
                  [ str model.Player.Name ]
                Text.p [ Modifiers [ ] ]
                  [ str "Welcome to the 2019/20 season"
                  ]
              ]
          ]

        // Components.cardWithFooter
        //   [ Message.message [ Message.Color IsInfo ]
        //       [ Message.body [ Modifiers [ Modifier.TextAlignment (Screen.Mobile, TextAlignment.Left) ] ]
        //           [ str "We are a work in progress so tell us your ideas and watch out for new features!"
        //           ]
        //       ]
        //   ]
        //   [ Card.Footer.a [ Props [ OnClick (fun _ -> dispatch ShowFeedbackModal) ] ]
        //       [ str "Feedback"
        //       ]
        //   ]
        (if model.IsSubscribable then
          Components.cardWithFooter
            [ Message.message [ Message.Color IsInfo ] [
                  Message.header [ ]
                    [ str "Notifications"
                      Delete.delete [ Delete.OnClick (fun _ -> dispatch DismissSubscribePrompt) ] []
                    ]
                  Message.body [ Modifiers [ Modifier.TextAlignment (Screen.Mobile, TextAlignment.Left) ] ]
                    [ str "Receive notifications on this device when fixtures are added and more news!"
                    ]
                ]
            ]
            [
              Card.Footer.a []
                [ subscribeButton model dispatch ]
            ]
        else
          div [] [])

        (match model.LeagueTable, model.TotalPoints with
        | Success league, Success points ->
          div [] (loadedView model (league, points) dispatch)
        | _ ->
          div [] [])

        Components.cardWithFooter []
          [ Card.Footer.a [ Props [ OnClick (fun _ -> dispatch Logout) ] ]
              [ str "Logout"
              ]
          ]

        feedbackModal dispatch model
      ]

  let init api player =
    { Player = player
      TotalPoints = Fetching
      LeagueTable = Fetching
      ShowFeedbackModal = false
      FeedbackText = ""
      IsSubscribable = false
      IsSubscribing = false
    }, Cmd.batch
        [ Cmd.OfAsync.perform
            (api.getPlayerPointsTotal player.Id)
            player.Token
            PlayerPointsTotalRecieved
          Cmd.OfAsync.perform
            (api.getLeagueTable GlobalLeague Full)
            player.Token
            LeagueTableReceived
          Cmd.OfPromise.perform
            isSubscribableToPush ()
            InitIsSubscribableReceived
        ]

  let safeDateTimeToString s =
    match DateTime.TryParse s with
    | true, d -> d
    | _ -> DateTime.MinValue

  let isOver60Days (d:DateTime) =
    d.Date.AddDays 60. < DateTime.Now.Date

  let dismissSubscriptionStorageKey =
    "dismiss-subscription"

  let update (api:IProtocol) player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | PlayerPointsTotalRecieved r -> { model with TotalPoints = resultToWebData r }, []
    | LeagueTableReceived r -> { model with LeagueTable = resultToWebData r }, []
    | ShowFeedbackModal -> { model with ShowFeedbackModal = true }, []
    | HideFeedbackModal -> { model with ShowFeedbackModal = false }, []
    | EditFeedback s -> { model with FeedbackText = s }, []
    | SubmitFeedback ->
      { model with ShowFeedbackModal = false; FeedbackText = "" },
        Cmd.OfAsync.perform
          (api.submitFeedback model.FeedbackText)
          player.Token
          (fun _ -> AlertInfo "Thanks for your feedback!")
    | NavTo r -> model, navTo r
    | AlertInfo s ->
      model, (Toast.message >> Toast.position Toast.TopCenter >> Toast.info) s
    | Logout _ -> model, []
    | InitIsSubscribableReceived isSubscribable ->
      localStorage.getItem dismissSubscriptionStorageKey
      |> safeDateTimeToString
      |> isOver60Days
      |> fun isDismissExpired ->
      { model with IsSubscribable = isSubscribable && isDismissExpired }, []
    | DismissSubscribePrompt ->
      localStorage.setItem(dismissSubscriptionStorageKey, DateTime.Now.Date.ToString("yyyy-MM-dd"))
      { model with IsSubscribable = false }, []
    | Subscribe ->
      { model with IsSubscribing = true },
        Cmd.OfPromise.perform
          subscribeToPush ()
          Subscribed
    | Subscribed sub ->
      localStorage.removeItem dismissSubscriptionStorageKey
      { model with IsSubscribing = false; IsSubscribable = false },
        Cmd.OfAsync.perform
          (api.subscribeToPush player.Token) sub
          (fun _ -> AlertInfo "Notifications enabled")
