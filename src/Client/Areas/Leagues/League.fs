namespace Areas.Leagues

open Elmish
open System

open Fable.React
open Fable.React.Props
open Fable.Import
open Fable.Core.JS

open Areas
open Shared
open Fulma
open Routes

module League =

  type Model =
    { PrivateLeagueId : PrivateLeagueId
      League : LeagueTableDoc WebData
      MaxGameweekNo : GameweekNo WebData
      ShowInviteModal : bool
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | MaxGwnoReceived of Rresult<GameweekNo>
    | NavTo of Route
    | ShowModal
    | HideModal

  let init api player privateleagueId =
    Cmd.batch
      [ Cmd.OfAsync.either
          (api.getLeagueTable (PrivateLeague privateleagueId) Full)
          player.Token
          LeagueReceived
          (Error >> Init)
        Cmd.OfAsync.either
          api.getMaxGameweekNo
          player.Token
          MaxGwnoReceived
          (Error >> Init)
      ]
    |> fun cmds ->
    { PrivateLeagueId = privateleagueId
      League = Fetching
      MaxGameweekNo = Fetching
      ShowInviteModal = false
    }, cmds

  let buildInviteLink (PrivateLeagueId leagueId) =
    sprintf "%s//%s/%s"
      Browser.Dom.window.location.protocol
      Browser.Dom.window.location.hostname
      // (if Browser.Dom.window.location.port = "80" then "" else sprintf ":%s" Browser.Dom.window.location.port)
      (string leagueId |> Routes.joinLeaguePath)

  let whatsAppLink =
    encodeURI
    >> fun uri ->
      a [ Href <| sprintf "whatsapp://send?text=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-whatsapp fa-lg" ] [] ]
          span [] [ str "Whatsapp" ]
        ]

  let facebookLink =
    encodeURI
    >> fun uri ->
      a [ Href <| sprintf "https://www.facebook.com/sharer/sharer.php?u=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-facebook fa-lg" ] [] ]
          span [] [ str "Facebook" ]
        ]

  let twitterLink =
    encodeURI
    >> fun uri ->
      a [ Href <| sprintf "https://twitter.com/intent/tweet?text=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-twitter fa-lg" ] [] ]
          span [] [ str "Twitter" ]
        ]

  let inviteModal (model:Model) dispatch =
    let inviteLink =
      buildInviteLink model.PrivateLeagueId
    Modal.modal [ Modal.IsActive model.ShowInviteModal ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch HideModal) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ]
              [ Heading.h5 [ Heading.IsSubtitle ] [ str "Invite to League" ]
                div [ Style [ MarginBottom "1em" ] ]
                  [ p [ Style [ MarginBottom "0.6em" ] ] [ str "Copy invite link" ]
                    a [ Href inviteLink ] [ str inviteLink ]
                  ]
                div []
                  [ p [] [ str "Or share using" ]
                  ]
                div []
                  [ facebookLink inviteLink
                  ]
                div []
                  [ twitterLink inviteLink
                  ]
                div []
                  [ whatsAppLink inviteLink
                  ]
              ]
          ]
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick (fun _ -> dispatch HideModal) ] [ ] ]

  let showPaymentPrompt (now:DateTime) leagueId =
    now.Date < new DateTime(2019, 9, 2)
      && leagueId = (Guid.Parse("f27fc62e-ab47-48f5-9b19-0569176704a2") |> PrivateLeagueId)

  let leagueView (league:LeagueTableDoc) (GameweekNo gwno) model dispatch =
    let (LeagueName name) =
      league.LeagueName
    let (PrivateLeagueId leagueId) =
      model.PrivateLeagueId
    let standingsFooter =
      Card.footer []
        [ Card.Footer.a [ Props [ OnClick (fun _ -> LeagueTableRoute (string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] ]
            [ str "Table"
            ]
          Card.Footer.a [ Props [ OnClick (fun _ -> LeagueMatrixRoute (string leagueId, gwno) |> LeaguesRoute |> NavTo |> dispatch) ] ]
            [ str "Latest Matrix"
            ]
          Card.Footer.a [ Props [ OnClick (fun _ -> LeagueHistoryRoute (string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] ]
            [ str "History"
            ]
        ]
    let membershipFooter =
      Card.footer []
        [ Card.Footer.a [ Props [ OnClick (fun _ -> LeaveLeagueRoute (string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] ]
            [ str "Leave"
            ]
          Card.Footer.a [ Props [ OnClick (fun _ -> dispatch ShowModal) ] ]
            [ str "Invite"
            ]
        ]
    div [ ClassName "block" ]
      [ Components.pageTitle name
        (if showPaymentPrompt DateTime.UtcNow model.PrivateLeagueId then
          Components.card
            [ Message.message [ Message.Color IsDanger ]
                [ Message.body [ Modifiers [ Modifier.TextAlignment (Screen.Mobile, TextAlignment.Left) ] ]
                    [ str "The payment deadline for PL1 is 1st September."
                      str " "
                      str "Please transfer £25 to"
                      str " "
                      span [ Style [ CSSProp.WhiteSpace "nowrap" ] ] [ str "39138556" ]
                      str " "
                      span [ Style [ CSSProp.WhiteSpace "nowrap" ] ] [ str "07-04-36" ]
                      str " "
                      str "to continue playing. All money received will be paid out as prize money. Cheers, Nick."
                    ]
                ]
            ]
          else
            div [] [])
        Components.subHeading "Standings"
        Card.card [ CustomClass "card-footer-only"; Props [ Style [ MarginBottom "2em" ] ] ]
          [ div [] [ standingsFooter ]
          ]
        Components.subHeading "Membership"
        Card.card [ CustomClass "card-footer-only" ]
          [ div [] [ membershipFooter ]
          ]
        div [] [ inviteModal model dispatch ]
      ]

  let view (model:Model) dispatch =
    match model.League, model.MaxGameweekNo with
    | Success league, Success gwno -> leagueView league gwno model dispatch
    | _ -> div [] [ str "could not find league" ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | MaxGwnoReceived r -> { model with MaxGameweekNo = resultToWebData r }, []
    | NavTo r -> model, navTo r
    | ShowModal -> { model with ShowInviteModal = true }, []
    | HideModal -> { model with ShowInviteModal = false }, []
