module Client

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Import
open Fable.Remoting.Client
open Fable.FontAwesome
open Fable.Core
open Fulma
open Thoth.Elmish
open Thoth.Elmish.Toast

open Routes
open Areas
open Areas.Gameweek
open Areas.Leagues
open Areas.Players
open Shared
open Shared.Routes
open Components

type Area =
  | LoginArea
  | HowItWorksArea
  | ContactArea
  | HomeArea of HomeArea.Model
  | GameweekArea of GameweekArea.Model
  | LeaguesArea of LeaguesArea.Model
  | PlayersArea of PlayersArea.Model

type Model =
  { Player: ClientSafePlayer option
    ErrorMsg: RemoteError option
    Area: Area }

type Msg =
  | Init of Result<string, exn>
  | NavTo of Route
  | HomeMsg of HomeArea.Msg
  | GameweekMsg of GameweekArea.Msg
  | LeaguesMsg of LeaguesArea.Msg
  | PlayersMsg of PlayersArea.Msg
  | Noop

let api: IProtocol =
  Remoting.createApi ()
  |> Remoting.withRouteBuilder Routes.builder
  |> Remoting.buildProxy<IProtocol>

let playerStorageKey = "player-2022/23"

[<Emit("window.navigator.standalone === true")>]
let isIosStandalone: bool = jsNative

let update msg (model: Model) : Model * Cmd<Msg> =
  // printfn "m %A" model
  // printfn "msg %A" msg
  // printfn "p %A" model.Player
  match model.Player, model.Area, msg with
  | _, _, NavTo r -> model, Cmd.batch [ navTo r; Cmd.OfFunc.perform Html.unClip () (fun _ -> Noop)  ]

  | Some _, PlayersArea _, PlayersMsg (PlayersArea.Msg.MyProfileMsg (MyProfile.Msg.Logout))
  | Some _, HomeArea _, HomeMsg (HomeArea.Msg.Logout) ->
    Browser.WebStorage.localStorage.removeItem playerStorageKey
    { model with Player = None }, navTo LoginRoute

  | Some p, HomeArea m, HomeMsg msg ->
    let m, cmd = HomeArea.update api p msg m
    { model with Area = HomeArea m }, Cmd.map HomeMsg cmd

  | Some p, GameweekArea m, GameweekMsg msg ->
    let m, cmd = GameweekArea.update api p msg m
    { model with Area = GameweekArea m }, Cmd.map GameweekMsg cmd

  | Some p, LeaguesArea m, LeaguesMsg msg ->
    let m, cmd = LeaguesArea.update api p msg m
    { model with Area = LeaguesArea m }, Cmd.map LeaguesMsg cmd

  | Some p, PlayersArea m, PlayersMsg msg ->
    let m, cmd = PlayersArea.update api p msg m
    { model with Area = PlayersArea m }, Cmd.map PlayersMsg cmd

  | _ -> model, []

let footabs model dispatch : ReactElement option =
  let desc s =
    Text.span [ Modifiers [ Modifier.IsHidden(Screen.Mobile, true) ]
                Props [ Style [ MarginLeft "5px" ] ] ] [
      str s
    ]

  let back =
    [ a [ Href "javascript:history.back();" ] [
        Fa.i [ Fa.Solid.ChevronLeft ] []
        desc "Back"
      ] ]

  let home =
    [ a
        (anchorNavProps (NavTo >> dispatch) HomeRoute)
        [ Fa.i [ Fa.Solid.Home ] []
          desc "Home" ] ]

  let gameweek =
    [ a
        (anchorNavProps (NavTo >> dispatch) (GameweekRoute GameweekInitRoute))
        [ Fa.i [ Fa.Regular.Futbol ] []
          desc "Gameweek" ] ]

  let leagues =
    [ a
        (anchorNavProps (NavTo >> dispatch) (LeaguesRoute PlayerLeaguesRoute))
        [ Fa.i [ Fa.Solid.Trophy ] []
          desc "Leagues" ] ]

  let players =
    [ a
        (anchorNavProps (NavTo >> dispatch) (PlayersRoute MyProfileRoute))
        [ Fa.i [ Fa.Regular.User ] []
          desc "My Profile" ] ]

  let tabs tabList =
    let backButtonTab =
      Tabs.tab [ Tabs.Tab.CustomClass "back-button" ] back

    Tabs.tabs
      [ Tabs.IsCentered
        Tabs.IsFullWidth
        Tabs.IsToggle
        Tabs.Size IsMedium ]
      ((if isIosStandalone then
          [ backButtonTab ]
        else
          [])
       @ tabList)
    |> Some

  match model.Area with
  | HowItWorksArea _
  | ContactArea _
  | LoginArea _ -> None
  | HomeArea _ ->
    tabs [ Tabs.tab [ Tabs.Tab.IsActive true ] home
           Tabs.tab [] gameweek
           Tabs.tab [] leagues
           Tabs.tab [] players ]
  | GameweekArea _ ->
    tabs [ Tabs.tab [] home
           Tabs.tab [ Tabs.Tab.IsActive true ] gameweek
           Tabs.tab [] leagues
           Tabs.tab [] players ]
  | LeaguesArea _ ->
    tabs [ Tabs.tab [] home
           Tabs.tab [] gameweek
           Tabs.tab [ Tabs.Tab.IsActive true ] leagues
           Tabs.tab [] players ]
  | PlayersArea _ ->
    tabs [ Tabs.tab [] home
           Tabs.tab [] gameweek
           Tabs.tab [] leagues
           Tabs.tab [ Tabs.Tab.IsActive true ] players ]

let logoBar =
  Navbar.navbar [ Navbar.Color IsPrimary
                  Navbar.Props [ Style [ MarginBottom "1em" ] ] ] [
    Navbar.Brand.div [] [
      div [ Style [ Padding "1em" ] ] [
        Heading.h5 [ Heading.Modifiers [ Modifier.TextColor IsWhite
                                         Modifier.TextTransform TextTransform.UpperCase ] ] [
          a [ Href "/"; Style [ Color "#fff" ] ] [
            str "Right Result"
          ]
        ]
      ]
    ]
  ]

let title model =
  match model.Area with
  | ContactArea _
  | HowItWorksArea _
  | HomeArea _ -> div [] []
  | _ -> logoBar

let navBar model dispatch =
  match footabs model dispatch with
  | Some tabs -> // tabs
    Navbar.navbar [ Navbar.IsFixedBottom ] [
      tabs
    ]
  | _ -> div [] []

let area model dispatch =
  match model.Area with
  | LoginArea -> LoginArea.view dispatch
  | HowItWorksArea -> HowItWorksArea.view dispatch
  | ContactArea -> ContactArea.view dispatch
  | HomeArea m -> HomeArea.view m (HomeMsg >> dispatch)
  | GameweekArea m -> GameweekArea.view m (GameweekMsg >> dispatch)
  | LeaguesArea m -> LeaguesArea.view m (LeaguesMsg >> dispatch)
  | PlayersArea m -> PlayersArea.view m (PlayersMsg >> dispatch)

let view (model: Model) dispatch =
  Container.container [] [
    Columns.columns [ Columns.IsDesktop; Columns.IsGapless ] [
      Column.column [ Column.Width(Screen.All, Column.Is6) ] [
        title model
        div [ Style [ CSSProp.MarginBottom "100px" ] ] [
          area model dispatch
        ]
        navBar model dispatch
      ]
    ]
  ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json

let clearFragment () = Browser.Dom.window.location.hash <- ""

let loadPlayerFromBrowserStorage () =
  try
    Browser.WebStorage.localStorage.getItem playerStorageKey
    |> Decode.fromString Decoders.decodeClientSafePlayer
    |> function
      | Ok p -> Some p
      | _ -> None
  with
  | _ -> None

let urlUpdate route model =
  match model.Player, route with
  | _, Some LoginRoute -> { model with Area = LoginArea }, Cmd.none
  | _, Some ContactRoute -> { model with Area = ContactArea }, Cmd.none
  | _, Some HowItWorksRoute -> { model with Area = HowItWorksArea }, Cmd.none
  | _, Some LoggedInRoute ->
    let loginInAndRedirect playerString nav =
      playerString
      |> JS.decodeURIComponent
      |> Browser.Dom.window.atob
      |> fun p -> Browser.WebStorage.localStorage.setItem (playerStorageKey, p)

      { model with
          Player = loadPlayerFromBrowserStorage () },
      nav

    match getFragmentValue "player", getFragmentValue "redirectPath" with
    | Some playerString, Some path when path <> "" -> loginInAndRedirect playerString (Navigation.newUrl path)
    | Some playerString, _ -> loginInAndRedirect playerString (navTo HomeRoute)
    | _ -> model, []

  | Some p, Some HomeRoute ->
    let m, cmd = HomeArea.init api p
    { model with Area = HomeArea m }, Cmd.map HomeMsg cmd

  | Some p, Some (GameweekRoute r) ->
    let m, cmd = GameweekArea.urlUpdate api p r
    { model with Area = GameweekArea m }, Cmd.map GameweekMsg cmd

  | Some p, Some (LeaguesRoute r) ->
    let m, cmd = LeaguesArea.urlUpdate api p r
    { model with Area = LeaguesArea m }, Cmd.map LeaguesMsg cmd

  | Some p, Some (PlayersRoute r) ->
    let m, cmd = PlayersArea.urlUpdate api p r
    { model with Area = PlayersArea m }, Cmd.map PlayersMsg cmd

  | _ ->
    let redirectPath = Browser.Dom.window.location.pathname
    (model, Navigation.modifyUrl (sprintf "/%s?%s=%s" Routes.loginPath redirectPathKey redirectPath))

let init route =
  let player = loadPlayerFromBrowserStorage ()

  if Option.isNone player then
    Browser.WebStorage.localStorage.removeItem playerStorageKey

  urlUpdate
    route
    { Player = player
      ErrorMsg = None
      Area = LoginArea }

Program.mkProgram init update view
|> Program.toNavigable (parsePath route) urlUpdate
|> Toast.Program.withToast Toast.render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
