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
      // (if Browser.Dom.window.location.port = "79" then "" else sprintf ":%s" Browser.Dom.window.location.port)
      (string leagueId |> Routes.joinLeaguePath)

  let whatsAppLink =
    encodeURI
    >> fun uri ->
      Menu.Item.li [ Menu.Item.Option.Href <| sprintf "whatsapp://send?text=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-whatsapp fa-lg" ] [] ]
          span [ ] [ str "WhatsApp" ]
        ]

  let facebookLink =
    encodeURI
    >> fun uri ->
      Menu.Item.li [ Menu.Item.Option.Href <| sprintf "https://www.facebook.com/sharer/sharer.php?u=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-facebook fa-lg" ] [] ]
          span [ ] [ str "Facebook" ]
        ]

  let twitterLink =
    encodeURI
    >> fun uri ->
      Menu.Item.li [ Menu.Item.Option.Href <| sprintf "https://twitter.com/intent/tweet?text=%s" uri ]
        [ span [ Class "icon" ] [ i [ Class "fab fa-twitter fa-lg" ] [] ]
          span [ ] [ str "Twitter" ]
        ]

  let inviteModal (model:Model) dispatch =
    let inviteLink =
      buildInviteLink model.PrivateLeagueId
    Modal.modal [ Modal.IsActive model.ShowInviteModal ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch HideModal) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ]
              [ Heading.h4 [ Heading.IsSubtitle ] [ str "Share invite link" ]
                div [ Style [ MarginBottom "0em" ] ]
                  [ a [ Href inviteLink ] [ str inviteLink ]
                  ]

                Menu.menu []
                  [ Menu.list [ ]
                      [ facebookLink inviteLink
                        twitterLink inviteLink
                        whatsAppLink inviteLink
                      ]
                  ]
              ]
          ]
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick (fun _ -> dispatch HideModal) ] [ ] ]

  let message (PrivateLeagueId leagueId) =
    if leagueId = Guid.Parse ("4c15bcf0-ba45-446c-bd4d-3e3a45301680") then
      Message.message [ Message.Color IsWarning ]
        [ Message.body [ ]
            [ str "Deadline for payment is 30/09"
              br []
              str "Please email"
              str " "
              a [ Href "mailto:predictionleague1@hotmail.com"] [ str "predictionleague1@hotmail.com" ]
            ] ]
    else div [] []

  let leagueView { LeagueTableDoc.LeagueName = LeagueName name } (GameweekNo gwno) model dispatch =
    let (PrivateLeagueId leagueId) =
      model.PrivateLeagueId
    let standingsFooter =
      Components.leagueMenu (string leagueId) gwno (NavTo >> dispatch)
    let membershipFooter =
      Components.card
        [ Menu.menu []
            [ Menu.list []
                [ Menu.Item.li [ Menu.Item.OnClick (fun _ -> LeaveLeagueRoute (string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] [ str "Leave" ]
                  Menu.Item.li [ Menu.Item.OnClick (fun _ -> dispatch ShowModal) ] [ str "Invite" ]
                ]
            ]
        ]
    div [ ClassName "block" ]
      [ Components.pageTitle name
        message model.PrivateLeagueId
        Components.subHeading "Standings"
        Card.card [ CustomClass "card-footer-only"; Props [ Style [ MarginBottom "1em" ] ] ]
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
