namespace Areas.Leagues

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared
open Fulma
open Routes
open Areas

module JoinLeague =

  type Model =
    { PrivateLeagueId : PrivateLeagueId
      League : LeagueTableDoc WebData
      IsLoading : bool }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | Confirm of PrivateLeagueId
    | ConfirmResult of Rresult<PrivateLeagueId>
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

  let joinLeagueButton (model:Model) dispatch =
    match model.IsLoading with
    | false -> button [] "Join" (fun _ -> Confirm model.PrivateLeagueId |> dispatch)
    | true  -> button [Button.IsLoading true] "" ignore

  let leagueView (league:LeagueTableDoc) model dispatch =
    let (LeagueName name) =
      league.LeagueName
    div []
      [ Components.pageTitle "Join league"
        Card.card []
          [ Card.content []
              [ p [] [ str <| sprintf "Confirm you wish to join %s" name ]
              ]
            Card.footer []
              [ Card.Footer.item [ Props [ OnClick (fun _ -> LeaguesRoute PlayerLeaguesRoute |> NavTo |> dispatch) ] ]
                  [ str "Cancel" ]
                Card.Footer.item []
                  [ joinLeagueButton model dispatch ]
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
        (api.joinLeague player.Token)
        privateLeagueId
        ConfirmResult
        (Error >> Init)
    | NavTo r ->
      model, (Routes.navTo r)
    | ConfirmResult r ->
      match r with
      | Ok (PrivateLeagueId privateLeagueId) ->
        model, Cmd.ofPromise delay (LeagueRoute (string privateLeagueId) |> LeaguesRoute) NavTo (Error >> Init)
      | Error e -> model, alert e
