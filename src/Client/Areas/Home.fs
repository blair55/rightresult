namespace Areas

open Elmish
open Elmish.React
open Thoth.Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes

module HomeArea =

  type Model =
    { Player : ClientSafePlayer
      TotalPoints : PredictionPointsMonoid WebData
      LeagueTable : LeagueTableDoc WebData
      ShowFeedbackModal : bool
      FeedbackText : string
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

  let button txt onClick =
    Button.button
      [ Button.IsFullWidth
        Button.OnClick onClick ]
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

  let view model dispatch =
    div []
      [ Components.pageTitle "Right Result"

        Components.card
          [ div []
              [ Text.p [ Modifiers [ Modifier.TextWeight TextWeight.SemiBold ] ]
                  [ str model.Player.Name ]
                Text.p [ Modifiers [ ] ]
                  [ str "Welcome to the 2018/19 season"
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
    }, Cmd.batch
        [ Cmd.OfAsync.either
            (api.getPlayerPointsTotal player.Id)
            player.Token
            PlayerPointsTotalRecieved
            (Error >> Init)
          Cmd.OfAsync.either
            (api.getLeagueTable GlobalLeague Full)
            player.Token
            LeagueTableReceived
            (Error >> Init)
        ]

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
        Cmd.OfAsync.either
          (api.submitFeedback model.FeedbackText)
          player.Token
          (fun _ -> AlertInfo "Thanks for your feedback!")
          (Error >> Init)
    | NavTo r -> model, navTo r
    | AlertInfo s ->
      model, (Toast.message >> Toast.position Toast.TopCenter >> Toast.info) s
    | Logout _ -> model, []
