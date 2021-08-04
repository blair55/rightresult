namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open Areas

module CreateLeague =

  type Model =
    { LeagueName : LeagueName
      IsLoading : bool }

  type Msg =
    | Init of Result<string, exn>
    | EditName of LeagueName
    | Create of LeagueName
    | CreateResult of Result<PrivateLeagueId, RemoteError>
    | NavTo of Route

  let init api player : Model * Cmd<Msg> =
    { LeagueName = LeagueName ""
      IsLoading = false }, Cmd.none

  let button attr txt onClick =
    Button.button
      ([ Button.IsFullWidth
         Button.Color IsPrimary
         Button.OnClick onClick ] @ attr)
      [ str txt ]

  let createLeagueButton args dispatch =
    match args with
    | "", false         -> button [Button.Disabled true]  "Create League" ignore
    | leagueName, false -> button []                      "Create League" (fun _ -> LeagueName leagueName |> Create |> dispatch)
    | _, true           -> button [Button.IsLoading true] ""              ignore

  let view { LeagueName = LeagueName leagueName; IsLoading = isLoading } dispatch =
    div []
      [ Components.pageTitle "Create League"
        Box.box' []
          [
            div [Class "block"]
                [ p [Style[LineHeight "1.5em"]] [ str "Invite your mates to compete in a private league." ]
                ]
            div [Class "block"]
              [ Input.text
                  [ Input.Value leagueName
                    Input.Placeholder "League name"
                    Input.OnChange (fun e -> LeagueName e.Value |> EditName |> dispatch) ]
              ]
            div [Class "block"]
                [ createLeagueButton (leagueName, isLoading) dispatch
                ]
            div [Class "block"]
              [ Card.Footer.a [ Props [ OnClick (fun _ -> LeaguesRoute PlayerLeaguesRoute |> NavTo |> dispatch) ] ]
                    [ str "Cancel" ]
              ]
          ]
      ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | EditName leagueName ->
      { model with LeagueName = leagueName }, []
    | Create leagueName ->
      { model with IsLoading = true },
      Cmd.OfAsync.either
        (api.createLeague player.Token)
        leagueName
        CreateResult
        (Error >> Init)
    | NavTo r ->
      model, (Routes.navTo r)
    | CreateResult r ->
      match r with
      | Ok (PrivateLeagueId leagueId) ->
        model, Cmd.OfPromise.either delay (LeagueRoute (string leagueId) |> LeaguesRoute) NavTo (Error >> Init)
      | Error e -> model, alert e
