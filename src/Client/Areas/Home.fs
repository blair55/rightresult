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
      GlobalGwWinner : WebData<GlobalGameweekWinner option>
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
    Button.button
      ([ Button.OnClick onClick ] @ attr)
      [ str txt ]

  let heroBar (model:Model) points =
    Hero.hero
      [ Hero.Color IsPrimary
        Hero.IsBold
        Hero.Props
          [ Style [ MarginBottom "1em" ] ]
      ]
      [ Hero.body []
          [ Container.container [ Container.IsFluid ]
              [ Heading.h1 [ Heading.Is3 ]
                  [ a [ Href "/"; Style [ Color "#fff" ] ] [ str "Right Result" ]
                    str " "
                    span [ ClassName "is-size-6" ] [ str "2019/20" ]
                  ]
                Components.pointsTotalView points
              ] ] ]

  let globalLeaguePositionView (model:Model) (league:LeagueTableDoc) dispatch =
    let (_, membr) =
      league.Members |> List.filter (fun (pId, _) -> pId = model.Player.Id) |> List.head
    div []
      [ str <| sprintf "Position %i" membr.Position
      ]

  let gwWinner dispatch = function
    | Some
      { GameweekNo = GameweekNo gwno
        PlayerId = PlayerId playerId
        Member = { LeagueTableMember.PlayerName = (PlayerName playerName); Points = m }
      } ->
      Hero.hero
        [ Hero.Color IsInfo
          Hero.IsBold
          Hero.Props
            [ Style [ MarginBottom "1em" ]
              OnClick (fun _ -> PlayerRoute playerId |> PlayersRoute |> NavTo |> dispatch)
            ]
        ]
        [ Hero.body []
            [ Container.container
                [ Container.IsFluid ]
                [ Heading.h3 []
                    [ str playerName ]
                  Heading.h6 []
                    [ str <| sprintf "Wins GW%i with %i points" gwno m.Points ] ] ] ]
    | None ->
      div [] []

  let notificationPrompt model dispatch =
    let subscribeButton (model:Model) dispatch =
      match model.IsSubscribing with
      | false -> button [Button.Color IsInfo] "Enable Notifications" (fun _ -> dispatch Subscribe)
      | true  -> button [Button.Color IsWhite; Button.IsLoading true] "" ignore
    if model.IsSubscribable then
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
        [ Card.Footer.a []
            [ subscribeButton model dispatch ]
        ]
    else
      div [] []

  let homeMenu model dispatch =
    let (PlayerId playerId) =
      model.Player.Id
    Components.card
      [ Menu.menu []
          [ Menu.list []
              [ Menu.Item.li [ Menu.Item.OnClick (fun _ -> LeaguesRoute GlobalLeagueRoute |> NavTo |> dispatch) ] [ str "Global League" ]
                Menu.Item.li [ Menu.Item.OnClick (fun _ -> PlayerRoute playerId |> PlayersRoute |> NavTo |> dispatch) ] [ str model.Player.Name ]
                Menu.Item.li [ Menu.Item.OnClick (fun _ -> Logout |> dispatch) ] [ str "Log out" ]
              ]
          ]
      ]

  let loadedView (model:Model) (league, points, winner) dispatch =
    [ heroBar model points
      notificationPrompt model dispatch
      gwWinner dispatch winner
      homeMenu model dispatch
    ]

  [<Emit("isSubscribableToPush()")>]
  let isSubscribableToPush () : JS.Promise<bool> =
    jsNative

  [<Emit("subscribeToPush()")>]
  let subscribeToPush () : JS.Promise<PushSubscription> =
    jsNative

  let noNetworkView model =
    Components.card
      [ Message.message [ Message.Color IsWarning ] [
            Message.body [ Modifiers [ Modifier.TextAlignment (Screen.Mobile, TextAlignment.Left) ] ]
              [ str "No network!"
              ]
          ]
      ]

  let view model dispatch =
    match model.LeagueTable, model.TotalPoints, model.GlobalGwWinner with
    | Success league, Success points, Success winner ->
      div [] (loadedView model (league, points, winner) dispatch)
    | WebError _, _, _
    | _, WebError _, _
    | _, _, WebError _ ->
      noNetworkView model
    | _ ->
      div [] []

  let init api player =
    { Player = player
      TotalPoints = Fetching
      LeagueTable = Fetching
      GlobalGwWinner = Fetching
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
          Cmd.OfAsync.perform
            api.getGlobalGameweekWinner
            player.Token
            GlobalGwWinnerReceived
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
    | GlobalGwWinnerReceived r -> { model with GlobalGwWinner = resultToWebData r }, []
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
