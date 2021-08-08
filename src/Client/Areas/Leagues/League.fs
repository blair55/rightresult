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
open Fable.FontAwesome

module League =

  type Model =
    { PrivateLeagueId: PrivateLeagueId
      League: LeagueTableDoc WebData
      ActiveGameweekNo: GameweekNo WebData
      ShowInviteModal: bool }

  type Msg =
    | Init of Result<string, exn>
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

  let whatsAppLink =
    encodeURI
    >> fun uri ->
         Menu.Item.li [ Menu.Item.Option.Href
                        <| Components.Social.whatsAppHref uri ] [
           Fa.i [ Fa.Size Fa.ISize.FaLarge
                  Fa.Brand.WhatsappSquare ] []
           span [ Style [ MarginLeft "5px" ] ] [
             str "WhatsApp"
           ]
         ]

  let facebookLink =
    encodeURI
    >> fun uri ->
         Menu.Item.li [ Menu.Item.Option.Href
                        <| Components.Social.facebookHref uri ] [
           Fa.i [ Fa.Size Fa.ISize.FaLarge
                  Fa.Brand.FacebookSquare ] []
           span [ Style [ MarginLeft "5px" ] ] [
             str "Facebook"
           ]
         ]

  let twitterLink =
    encodeURI
    >> fun uri ->
         Menu.Item.li [ Menu.Item.Option.Href
                        <| Components.Social.twitterHref uri ] [
           Fa.i [ Fa.Size Fa.ISize.FaLarge
                  Fa.Brand.TwitterSquare ] []
           span [ Style [ MarginLeft "5px" ] ] [
             str "Twitter"
           ]
         ]

  let inviteModal (model: Model) dispatch =
    let inviteLink = buildInviteLink model.PrivateLeagueId

    Modal.modal [ Modal.IsActive model.ShowInviteModal ] [
      Modal.background [ Props [ OnClick(fun _ -> dispatch HideModal) ] ] []
      Modal.content [] [
        Box.box' [] [
          Components.subHeading "Share invite link"
          div [ Style [ MarginBottom "1em" ] ] [
            a [ Href inviteLink ] [ str inviteLink ]
          ]

          Menu.menu [] [
            Menu.list [] [
              facebookLink inviteLink
              twitterLink inviteLink
              whatsAppLink inviteLink
            ]
          ]
        ]
      ]
      Modal.close [ Modal.Close.Size IsLarge
                    Modal.Close.OnClick(fun _ -> dispatch HideModal) ] []
    ]

  let leagueView { LeagueTableDoc.LeagueName = LeagueName name } (GameweekNo gwno) model dispatch =
    let (PrivateLeagueId leagueId) = model.PrivateLeagueId

    let standingsFooter =
      Components.leagueMenu (string leagueId) gwno (NavTo >> dispatch)

    let membershipFooter =
      Panel.panel [ Panel.Color IsPrimary ] [
        Panel.Block.div [ ] [
          Panel.icon [] [
            Fa.i [Fa.Solid.UserFriends] []
          ]
          a [ OnClick(fun _ -> dispatch ShowModal) ] [ str  "Invite"]
        ]
        Panel.Block.a [] [
          Panel.icon [] [
            Fa.i [Fa.Solid.History] []
          ]
          a [ OnClick(fun _ -> LeaveLeagueRoute(string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] [ str  "Leave"]
        ]
      ]

    div [ ClassName "block" ] [
      Components.pageTitle name
      Components.subHeading "Standings"
      Card.card [ CustomClass "card-footer-only"
                  Props [ Style [ MarginBottom "1em" ] ] ] [
        div [] [ standingsFooter ]
      ]
      Components.subHeading "Membership"
      Card.card [ CustomClass "card-footer-only" ] [
        div [] [ membershipFooter ]
      ]
      div [] [ inviteModal model dispatch ]
    ]

  let view (model: Model) dispatch =
    match model.League, model.ActiveGameweekNo with
    | Success league, Success gwno -> leagueView league gwno model dispatch
    | WebError _, _
    | _, WebError _ -> div [] [ str "could not find league" ]
    | _ -> div [] [  ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r ->
      { model with
          League = resultToWebData r },
      []
    | ActiveGwnoReceived r ->
      { model with
          ActiveGameweekNo = resultToWebData r },
      []
    | NavTo r -> model, navTo r
    | ShowModal -> { model with ShowInviteModal = true }, []
    | HideModal -> { model with ShowInviteModal = false }, []
