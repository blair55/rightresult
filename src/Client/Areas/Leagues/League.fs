namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props
open Fable.Import

open Areas
open Shared
open Fulma
open Routes
open Fable.FontAwesome

module League =

  type Model =
    { PrivateLeagueId: PrivateLeagueId
      League: LeagueTableDoc WebData
      ActiveGameweekNo: GameweekNo WebData
      ShowInviteModal: bool }

  type Msg =
    | Init of Result<string, exn>
    | Noop
    | LeagueReceived of Rresult<LeagueTableDoc>
    | ActiveGwnoReceived of Rresult<GameweekNo>
    | NavTo of Route
    | ShowModal
    | HideModal

  let init api player privateleagueId =
    Cmd.batch [ Cmd.OfAsync.either
                  (api.getLeagueTable (PrivateLeague privateleagueId) Full)
                  player.Token
                  LeagueReceived
                  (Error >> Init)
                Cmd.OfAsync.either api.getEarliestOpenGwno player.Token ActiveGwnoReceived (Error >> Init) ]
    |> fun cmds ->
         { PrivateLeagueId = privateleagueId
           League = Fetching
           ActiveGameweekNo = Fetching
           ShowInviteModal = false },
         cmds

  let buildInviteLink (PrivateLeagueId leagueId) =
    sprintf
      "%s//%s/%s"
      Browser.Dom.window.location.protocol
      Browser.Dom.window.location.hostname
      // (if Browser.Dom.window.location.port = "79" then "" else sprintf ":%s" Browser.Dom.window.location.port)
      (string leagueId |> Routes.joinLeaguePath)

  let inviteModal (model: Model) dispatch =
    let inviteLink = buildInviteLink model.PrivateLeagueId

    Modal.modal [ Modal.IsActive model.ShowInviteModal ] [
      Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
      Modal.Card.card [] [
        Modal.Card.body [ Props [ Style [ Padding "1em 0" ] ] ] [
          Content.content [] [
            Components.subHeading "Share Invite Link"
            Panel.panel [ Panel.Color IsPrimary ] [
              Components.panelAnchorExternalUrl Fa.Solid.ShareSquare inviteLink inviteLink
              Components.panelAnchorExternalUrl
                Fa.Brand.FacebookSquare
                "facebook"
                (Components.Social.facebookHref inviteLink)
              Components.panelAnchorExternalUrl
                Fa.Brand.TwitterSquare
                "twitter"
                (Components.Social.twitterHref inviteLink)
              Components.panelAnchorExternalUrl
                Fa.Brand.WhatsappSquare
                "whatsapp"
                (Components.Social.whatsAppHref inviteLink)
            ]
          ]
        ]
      ]
      Modal.close [ Modal.Close.Size IsLarge
                    Modal.Close.OnClick(fun _ -> dispatch HideModal) ] []
    ]

  let whatsAppGroup leagueId =
    if leagueId = CashLeague.identifier then
      Box.box' [ Props [ Style [ MarginBottom "2em" ] ] ] [
        Button.a
          ([ Button.IsFullWidth
             Button.CustomClass "whatsapp-button"
             Button.IsLink
             Button.Props [ Href "https://chat.whatsapp.com/KkBqratVHJI1xWKmYYI51M" ] ])
          [ span [ Style [ MarginRight "3px" ] ] [
              Fa.i [ Fa.Brand.Whatsapp ] []
            ]
            str "Join WhatsApp Group" ]
      ]
    else
      div [] []

  let leagueView { LeagueTableDoc.LeagueName = LeagueName name } (GameweekNo gwno) model dispatch =
    let (PrivateLeagueId leagueId) = model.PrivateLeagueId

    let standingsFooter =
      Components.leagueMenu (string leagueId) gwno (NavTo >> dispatch)

    let membershipFooter =
      Panel.panel [ Panel.Color IsPrimary ] [
        Panel.Block.div [ Panel.Block.Props [ OnClick(fun _ -> dispatch ShowModal) ] ] [
          Panel.icon [] [
            Fa.i [ Fa.Solid.UserFriends ] []
          ]
          str "Invite to join League"
        ]
        Components.panelAnchor
          Fa.Solid.DoorOpen
          "Leave League"
          (NavTo >> dispatch)
          (LeaguesRoute(LeaveLeagueRoute(string leagueId)))
      ]

    div [ ClassName "block" ] [
      Components.pageTitle name
      Card.card [ CustomClass "card-footer-only"
                  Props [ Style [ MarginBottom "1em" ] ] ] [
        div [] [ membershipFooter ]
      ]
      whatsAppGroup model.PrivateLeagueId
      Card.card [ CustomClass "card-footer-only" ] [
        div [] [ standingsFooter ]
      ]
      div [] [ inviteModal model dispatch ]
    ]

  let view (model: Model) dispatch =
    match model.League, model.ActiveGameweekNo with
    | Success league, Success gwno -> leagueView league gwno model dispatch
    | WebError _, _
    | _, WebError _ -> div [] [ str "could not find league" ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | Noop _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | ActiveGwnoReceived r -> { model with ActiveGameweekNo = resultToWebData r }, []
    | NavTo r -> model, navTo r
    | ShowModal -> { model with ShowInviteModal = true }, Cmd.OfFunc.perform Html.clip () (fun _ -> Noop)
    | HideModal -> { model with ShowInviteModal = false }, Cmd.OfFunc.perform Html.unClip () (fun _ -> Noop)