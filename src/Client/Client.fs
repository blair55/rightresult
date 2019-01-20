module Client

open System
open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.Import
open Fable.Remoting.Client
open Fulma
open Fulma.FontAwesome
open Thoth.Elmish
open Thoth.Elmish.Toast

open Routes
open Areas
open Areas.Fixtures
open Areas.Leagues
open Areas.Players
open Shared
open Shared.Routes
open Components

type Area =
  | LoginArea
  | HomeArea of HomeArea.Model
  | FixturesArea of FixturesArea.Model
  | LeaguesArea of LeaguesArea.Model
  | PlayersArea of PlayersArea.Model

type Model =
  { Player : ClientSafePlayer option
    ErrorMsg : RemoteError option
    Area : Area }

type Msg =
  | Init of Result<string, exn>
  | NavTo of Route
  | HomeMsg of HomeArea.Msg
  | FixturesMsg of FixturesArea.Msg
  | LeaguesMsg of LeaguesArea.Msg
  | PlayersMsg of PlayersArea.Msg

let api : IProtocol =
  Proxy.createWithBuilder<IProtocol> Routes.builder

let update msg (model:Model) : Model * Cmd<Msg> =
  // printfn "m %A" model
  // printfn "msg %A" msg
  // printfn "p %A" model.Player
  match model.Player, model.Area, msg with
  | _, _, NavTo r -> model, navTo r

  | Some _, HomeArea _, HomeMsg (HomeArea.Msg.Logout) ->
    BrowserLocalStorage.delete "player"
    { model with Player = None }, navTo LoginRoute

  | Some p, HomeArea m, HomeMsg msg ->
    let m, cmd = HomeArea.update api p msg m
    { model with Area = HomeArea m }, Cmd.map HomeMsg cmd

  | Some p, FixturesArea m, FixturesMsg msg ->
    let m, cmd = FixturesArea.update api p msg m
    { model with Area = FixturesArea m }, Cmd.map FixturesMsg cmd

  | Some p, LeaguesArea m, LeaguesMsg msg ->
    let m, cmd = LeaguesArea.update api p msg m
    { model with Area = LeaguesArea m }, Cmd.map LeaguesMsg cmd

  | Some p, PlayersArea m, PlayersMsg msg ->
    let m, cmd = PlayersArea.update api p msg m
    { model with Area = PlayersArea m }, Cmd.map PlayersMsg cmd

  | _ -> model, alert (LoginProblem "No user found")

let footabs model dispatch : React.ReactElement =
  let desc s =
    Text.span [ Modifiers [ Modifier.IsHidden (Screen.Mobile, true) ] ] [ str s ]

  let home =
    [ a [ OnClick (fun _ -> NavTo HomeRoute |> dispatch) ]
        [ icon Fa.I.Home
          desc "Home"
        ]
    ]
  let fixtures =
    [ a [ OnClick (fun _ -> NavTo (FixtureRoute OmniFixturesRoute) |> dispatch) ]
        [ icon (Fa.I.Custom "fa-calendar-alt")
          desc "Fixtures"
        ]
    ]
  let leagues =
    [ a [ OnClick (fun _ -> NavTo (LeaguesRoute PlayerLeaguesRoute) |> dispatch) ]
        [ icon Fa.I.Trophy
          desc "Leagues"
        ]
    ]
  let players =
    [ a [ OnClick (fun _ -> NavTo (PlayersRoute AllPlayersRoute) |> dispatch) ]
        [ icon Fa.I.User
          desc "Players"
        ]
    ]
  let tabs = Tabs.tabs [ Tabs.IsCentered; Tabs.IsFullWidth ]
  match model.Area with
  | LoginArea _ -> div [] []
  | HomeArea _ ->
    tabs
      [ Tabs.tab [ Tabs.Tab.IsActive true ] home
        Tabs.tab [] fixtures
        Tabs.tab [] leagues
        Tabs.tab [] players ]
  | FixturesArea _ ->
    tabs
      [ Tabs.tab [] home
        Tabs.tab [ Tabs.Tab.IsActive true ] fixtures
        Tabs.tab [] leagues
        Tabs.tab [] players ]
  | LeaguesArea _ ->
    tabs
      [ Tabs.tab [] home
        Tabs.tab [] fixtures
        Tabs.tab [ Tabs.Tab.IsActive true ] leagues
        Tabs.tab [] players ]
  | PlayersArea _ ->
    tabs
      [ Tabs.tab [] home
        Tabs.tab [] fixtures
        Tabs.tab [] leagues
        Tabs.tab [ Tabs.Tab.IsActive true ] players ]

let logoBar =
  Navbar.navbar [Navbar.Color IsPrimary; Navbar.HasShadow ]
    [ Navbar.Brand.div []
        [ div [ Style [ Padding "1em" ] ]
            [ Heading.h5 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ]
                [ a [ Href "/"; Style [ Color "#fff" ] ] [ str "Right Result" ]
                ]
            ]
        ]
    ]

let navBar model dispatch =
  Navbar.navbar [ Navbar.IsFixedBottom; Navbar.HasShadow ]
    [ footabs model dispatch ]

let area model dispatch =
  match model.Area with
  | LoginArea      -> LoginArea.view dispatch
  | HomeArea m     -> HomeArea.view m (HomeMsg >> dispatch)
  | FixturesArea m -> FixturesArea.view m (FixturesMsg >> dispatch)
  | LeaguesArea m  -> LeaguesArea.view m (LeaguesMsg >> dispatch)
  | PlayersArea m  -> PlayersArea.view m (PlayersMsg >> dispatch)

let view (model:Model) dispatch =
  Container.container [ ]
    [ Columns.columns [Columns.IsDesktop; Columns.IsGapless]
        [ Column.column [ Column.Width (Screen.All, Column.Is6) ]
            [ logoBar
              div [ Style [ CSSProp.MarginBottom "100px" ] ]
                [ area model dispatch ]
              navBar model dispatch
            ]
        ]
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

let clearFragment () =
  Browser.window.location.hash <- ""

let urlUpdate route model =
  match model.Player, route with
  | _, Some LoginRoute ->
    { model with Area = LoginArea }, Cmd.none
  | _, Some LoggedInRoute ->
      let loginInAndRedirect playerString nav =
        playerString
        |> JS.decodeURIComponent
        |> Browser.window.atob
        |> fun p -> Browser.localStorage.setItem("player", p)
        { model with Player = BrowserLocalStorage.load<ClientSafePlayer> "player" }, nav
      match getFragmentValue "player", getFragmentValue "redirectPath" with
      | Some playerString, Some path when path <> "" -> loginInAndRedirect playerString (navToString path)
      | Some playerString, _ -> loginInAndRedirect playerString (navTo HomeRoute)
      | _ -> model, []

  | Some p, Some HomeRoute ->
    let m, cmd = HomeArea.init api p
    { model with Area = HomeArea m }, Cmd.map HomeMsg cmd

  | Some p, Some (FixtureRoute r) ->
    let m, cmd = FixturesArea.urlUpdate api p r
    { model with Area = FixturesArea m }, Cmd.map FixturesMsg cmd

  | Some p, Some (LeaguesRoute r) ->
    let m, cmd = LeaguesArea.urlUpdate api p r
    { model with Area = LeaguesArea m }, Cmd.map LeaguesMsg cmd

  | Some p, Some (PlayersRoute r) ->
    let m, cmd = PlayersArea.urlUpdate api p r
    { model with Area = PlayersArea m }, Cmd.map PlayersMsg cmd

  | _ ->
    let redirectPath = Browser.window.location.pathname
    (model, Navigation.modifyUrl (sprintf "/%s?%s=%s" Routes.loginPath redirectPathKey redirectPath))

let init route =
  let player =
    try BrowserLocalStorage.load<ClientSafePlayer> "player"
    with _ -> BrowserLocalStorage.delete "player"; None
  urlUpdate route
    { Player = player
      ErrorMsg = None
      Area = LoginArea }

Program.mkProgram init update view
|> Program.toNavigable (parsePath route) urlUpdate
|> Toast.Program.withToast Toast.render
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
