namespace Areas.Leagues

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared
open Fulma
open Routes
open Areas

module LeaveLeague =

  type Model =
    { PrivateLeagueId : PrivateLeagueId
      League : LeagueTableDoc WebData
      IsLoading : bool }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | Confirm of PrivateLeagueId
    | ConfirmResult of Rresult<Unit>
    | NavTo of Route

  let init api player privateLeagueId =
    { PrivateLeagueId = privateLeagueId
      League = Fetching
      IsLoading = false },
      Cmd.ofAsync
        (api.getLeagueTable (PrivateLeague privateLeagueId) Full)
        player.Token
        LeagueReceived
        (Error >> Init)

  let button attr txt onClick =
    Button.button
      ([ Button.IsFullWidth
         Button.Color IsPrimary
         Button.OnClick onClick ] @ attr)
      [ str txt ]

  let leaveLeagueButton (model:Model) dispatch =
    match model.IsLoading with
    | false -> button [] "Leave" (fun _ -> Confirm model.PrivateLeagueId |> dispatch)
    | true  -> button [Button.IsLoading true] "" ignore

  let leagueView (league:LeagueTableDoc) model dispatch =
    let (LeagueName name) =
      league.LeagueName
    let (PrivateLeagueId leagueId) =
      model.PrivateLeagueId
    div []
      [ Components.pageTitle "Leave league"
        Card.card []
          [ Card.content []
              [ p [] [ str <| sprintf "Confirm you wish to leave %s" name ]
              ]
            Card.footer []
              [ Card.Footer.a [ Props [ OnClick (fun _ -> LeagueRoute (string leagueId) |> LeaguesRoute |> NavTo |> dispatch) ] ]
                  [ str "Cancel" ]
                Card.Footer.a []
                  [ leaveLeagueButton model dispatch ]
              ]
          ]
      ]

  let view (model:Model) dispatch =
    match model.League with
    | NotAsked
    | Fetching -> div [] []
    | WebError _ -> div [] [ str "Could not find league" ]
    | Success league -> leagueView league model dispatch

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | Confirm privateLeagueId ->
      { model with IsLoading = true },
      Cmd.ofAsync
        (api.leaveLeague player.Token)
        privateLeagueId
        ConfirmResult
        (Error >> Init)
    | NavTo r ->
      model, (Routes.navTo r)
    | ConfirmResult r ->
      match r with
      | Ok () ->
        model, Cmd.ofPromise delay (PlayerLeaguesRoute |> LeaguesRoute) NavTo (Error >> Init)
      | Error e -> model, alert e
