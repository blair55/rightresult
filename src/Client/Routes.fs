module Routes

open Elmish.UrlParser
open Elmish.Navigation

type Route =
  | LoginRoute
  | HomeRoute
  | HowItWorksRoute
  | ContactRoute
  // | FixtureRoute of FixtureRoute
  | GameweekRoute of GameweekRoute
  | LeaguesRoute of LeaguesRoute
  | LoggedInRoute
  | PlayersRoute of PlayersRoute
// and FixtureRoute =
  // | OmniFixturesRoute
  // | AddFixtureSetRoute
and GameweekRoute =
  | GameweekInitRoute
  | GameweekFixturesRoute of int
  | AddGameweekRoute
and LeaguesRoute =
  | PlayerLeaguesRoute
  | GlobalLeagueRoute
  | CreateLeagueRoute
  | LeagueRoute of string
  | LeagueTableRoute of string
  | JoinLeagueRoute of string
  | LeaveLeagueRoute of string
  | LeagueHistoryRoute of string
  | LeagueHistoryFixtureSetRoute of string * int
  | LeagueHistoryMonthRoute of string * int * int
  | LeagueMatrixRoute of string * int
  | LeaguePremTableRoute of string
and PlayersRoute =
  | MyProfileRoute
  | AllPlayersRoute
  | PlayerRoute of string
  | PlayerFixtureSetRoute of string * string

let homePath          = ""
let loginPath         = "login"
let loggedInPath      = "logged-in"
let howItWorksPath    = "how-it-works"
let contactPath       = "get-in-touch"
// let fixturesPath      = "fixtures"
// let addFixtureSetPath = "fixtures/add"
let gwPath            = "gameweek"
let gwFixturesPath    = sprintf "gameweek/%i"
let addGameweekPath   = "gameweek/add"
let leaguesPath       = "leagues"
let globalleaguePath  = "leagues/global"
let createLeaguePath  = "leagues/create"
let leaguePath        = sprintf "leagues/%s"
let leagueTablePath   = sprintf "leagues/%s/table"
let joinLeaguePath    = sprintf "leagues/%s/join"
let leaveLeaguePath   = sprintf "leagues/%s/leave"
let leagueHistoryPath = sprintf "leagues/%s/history"
let leagueHistoryFsPath = sprintf "leagues/%s/history/gw/%i"
let leagueHistoryMnPath = sprintf "leagues/%s/history/month/%i/%i"
let leagueMatrixPath    = sprintf "leagues/%s/matrix/%i"
let leaguePremTablePath = sprintf "leagues/premtable/%s"
let playerProfilePath = "me"
let playersPath       = "players"
let playerPath        = sprintf "players/%s"
let playerFixtureSetPath = sprintf "players/%s/gameweek/%s"

let curry2 f x y = f (x, y) // convert tupled form function of two arguments into curried form
let curry3 f x y z = f (x, y, z) // convert tupled form function of two arguments into curried form

let route : Parser<Route -> Route, _> =
  oneOf [
    map HomeRoute          top
    map LoginRoute         (s loginPath)
    map LoggedInRoute      (s loggedInPath)
    map HowItWorksRoute    (s howItWorksPath)
    map ContactRoute       (s contactPath)
    // map (OmniFixturesRoute  |> FixtureRoute) (s fixturesPath)
    // map (AddFixtureSetRoute |> FixtureRoute) (s fixturesPath </> s "add")
    map (GameweekInitRoute  |> GameweekRoute) (s gwPath)
    map (AddGameweekRoute   |> GameweekRoute) (s gwPath </> s "add")
    map (GameweekFixturesRoute >> GameweekRoute) (s gwPath </> i32)
    map (PlayerLeaguesRoute |> LeaguesRoute) (s leaguesPath)
    map (GlobalLeagueRoute  |> LeaguesRoute) (s leaguesPath </> s "global")
    map (CreateLeagueRoute  |> LeaguesRoute) (s leaguesPath </> s "create")
    map (LeagueRoute        >> LeaguesRoute) (s leaguesPath </> str)
    map (LeagueTableRoute   >> LeaguesRoute) (s leaguesPath </> str </> s "table")
    map (JoinLeagueRoute    >> LeaguesRoute) (s leaguesPath </> str </> s "join")
    map (LeaveLeagueRoute   >> LeaguesRoute) (s leaguesPath </> str </> s "leave")
    map (LeagueHistoryRoute >> LeaguesRoute) (s leaguesPath </> str </> s "history")
    map (curry2 (LeagueHistoryFixtureSetRoute >> LeaguesRoute)) (s leaguesPath </> str </> s "history" </> s "gw" </> i32)
    map (curry3 (LeagueHistoryMonthRoute      >> LeaguesRoute)) (s leaguesPath </> str </> s "history" </> s "month" </> i32 </> i32)
    map (curry2 (LeagueMatrixRoute            >> LeaguesRoute)) (s leaguesPath </> str </> s "matrix"  </> i32)
    map (LeaguePremTableRoute >> LeaguesRoute) (s leaguesPath </> s "premtable" </> str)
    map (MyProfileRoute     |> PlayersRoute) (s playerProfilePath)
    map (AllPlayersRoute    |> PlayersRoute) (s playersPath)
    map (PlayerRoute        >> PlayersRoute) (s playersPath </> str)
    map (curry2 (PlayerFixtureSetRoute >> PlayersRoute)) (s playersPath </> str </> s "gameweek" </> str)
  ]

let private routeToPath = function
  | HomeRoute          -> homePath
  | LoginRoute         -> loginPath
  | LoggedInRoute      -> loggedInPath
  | HowItWorksRoute    -> howItWorksPath
  | ContactRoute       -> contactPath
  // | FixtureRoute r ->
    // match r with
    // | OmniFixturesRoute      -> fixturesPath
    // | AddFixtureSetRoute     -> addFixtureSetPath
  | GameweekRoute r ->
    match r with
    | GameweekInitRoute         -> gwPath
    | GameweekFixturesRoute gw  -> gwFixturesPath gw
    | AddGameweekRoute          -> addGameweekPath
  | LeaguesRoute r ->
    match r with
    | PlayerLeaguesRoute        -> leaguesPath
    | GlobalLeagueRoute         -> globalleaguePath
    | CreateLeagueRoute         -> createLeaguePath
    | LeagueRoute leagueId      -> leaguePath leagueId
    | LeagueTableRoute leagueId -> leagueTablePath leagueId
    | JoinLeagueRoute leagueId  -> joinLeaguePath leagueId
    | LeaveLeagueRoute leagueId -> leaveLeaguePath leagueId
    | LeagueHistoryRoute leagueId -> leagueHistoryPath leagueId
    | LeagueHistoryFixtureSetRoute (leagueId, gwno)   -> leagueHistoryFsPath leagueId gwno
    | LeagueHistoryMonthRoute (leagueId, year, month) -> leagueHistoryMnPath leagueId year month
    | LeagueMatrixRoute (leagueId, gwno)              -> leagueMatrixPath    leagueId gwno
    | LeaguePremTableRoute table -> leaguePremTablePath table
  | PlayersRoute r ->
    match r with
    | MyProfileRoute           -> playerProfilePath
    | AllPlayersRoute          -> playersPath
    | PlayerRoute playerId     -> playerPath playerId
    | PlayerFixtureSetRoute (pId, fsId) -> playerFixtureSetPath pId fsId

let navTo r =
  (routeToPath >> sprintf "/%s" >> Navigation.newUrl) r

open Fable.React

let href =
   routeToPath >> sprintf "/%s" >> Props.HTMLAttr.Href

let pushTo r =
  (routeToPath >> sprintf "/%s" >> Navigation.modifyUrl) r

let isValidGuid (g:string) =
  System.Guid.TryParse g |> fst

let toGuid (g:string) =
  System.Guid.Parse g

open Shared

type WebData<'a> =
  | NotAsked
  | Fetching
  | Success of 'a
  | WebError of RemoteError

module WebData =

  let map f = function
    | Success wd -> f wd |> Success
    | wd -> wd

let resultToWebData = function
  | Ok a    -> Success a
  | Error e -> WebError e

let resultToOption = function
  | Ok a    -> Some a
  | Error _ -> None

let delay r = promise {
  do! Promise.sleep 2000
  return r }

open Thoth.Elmish

let warningAlert s =
  (Toast.message >> Toast.position Toast.TopCenter >> Toast.warning) s

let alert = function
  | InvalidToken ->
    warningAlert "Could not login"
  | AsyncError e ->
    warningAlert e.Message
  | LoginProblem s
  | ValidationError s
  | ServerSideError s
  | StateTransitionError s
  | CommandApplicationError s
  | WrongEventVersionError s ->
    warningAlert s

let infoAlert s =
  (Toast.message >> Toast.position Toast.TopCenter >> Toast.info) s

open Fable.Import

let getQsValueFromString key (src:string) =
  src.Split('&')
  |> Array.map(fun kvp -> kvp.Split('=') |> fun arr -> arr.[0], arr.[1])
  |> Array.tryFind(fun (k, _) -> k = key)
  |> Option.map snd

let getQueryStringValue (key:string) =
  let href = Browser.Dom.window.location.href
  if href.Contains("?")
  then href.Split('?').[1] |> getQsValueFromString key
  else None

let getFragmentValue (key:string) =
  Browser.Dom.window.location.hash.TrimStart('#').Split('&')
  |> Array.map(fun kvp -> kvp.Split('=') |> fun arr -> arr.[0], arr.[1])
  |> Array.tryFind(fun (k, _) -> k = key)
  |> FSharp.Core.Option.map snd
