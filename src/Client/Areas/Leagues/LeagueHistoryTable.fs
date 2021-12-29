namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open System
open Fable.FontAwesome
open Shared
open Fulma
open Routes
open Areas

module LeagueHistoryTable =

  type Model =
    { Window: LeagueWindow
      WindowDescription: string WebData
      LeagueTable: LeagueTableDoc WebData
      Player: ClientSafePlayer }

  type Msg =
    | Init of Result<string, exn>
    | LeagueTableReceived of Rresult<LeagueTableDoc>
    | LeagueWindowDescriptionReceived of Rresult<string>
    | NavTo of Route

  let init api player window leagueId =
    { Window = window
      WindowDescription =
        match window with
        | Week (GameweekNo w) -> sprintf "Gameweek %i" w |> Success
        | Month _ -> Fetching
        | _ -> NotAsked
      LeagueTable = Fetching
      Player = player },
    Cmd.batch (
      [ Cmd.OfAsync.either (api.getLeagueTable leagueId window) player.Token LeagueTableReceived (Error >> Init) ]
      @ match window with
        | Month (YearMonth (y, m)) ->
          [ Cmd.OfAsync.either
              /// use (y, m, 2) to prevent against utc vs gmt+0100 issue
              (api.getDateFormat (DateTime(y, m, 2)) "MMMM yyyy")
              player.Token
              LeagueWindowDescriptionReceived
              (Error >> Init) ]
        | _ -> []
    )

  let menuLinks (league: LeagueTableDoc) (model: Model) =
    match model.Window with
    | Week (GameweekNo gwno) ->
      [ Fa.Solid.Th,
        $"Gameweek {gwno} Matrix",
        LeaguesRoute(LeagueMatrixRoute(Components.leagueIdStr league.LeagueId, gwno)) ]
    | _ -> []
    @ match league.LeagueTableScope with
      | Some (OfMonth (dt, desc)) ->
        [ Fa.Solid.ThList,
          $"{desc} Table",
          LeaguesRoute(LeagueHistoryMonthRoute(Components.leagueIdStr league.LeagueId, dt.Year, dt.Month)) ]
      | Some (IncludesGameweeks gwnos) ->
        gwnos
        |> List.map
             (fun (GameweekNo gwno) ->
               Fa.Solid.ThList,
               $"Gameweek {gwno} Table",
               LeaguesRoute(LeagueHistoryFixtureSetRoute(Components.leagueIdStr league.LeagueId, gwno)))
      | _ -> []
      @ [ Fa.Solid.History, "History", LeaguesRoute(LeagueHistoryRoute(Components.leagueIdStr league.LeagueId))
          Fa.Solid.Trophy, "League", LeaguesRoute(LeagueRoute(Components.leagueIdStr league.LeagueId)) ]

  let leagueView dispatch (league: LeagueTableDoc) desc (model: Model) =
    let (LeagueName name) = league.LeagueName

    div [ ClassName "block" ] [
      Components.pageTitle name
      Components.subHeading $"{desc} Table"
      Card.card [] [
        Components.table (NavTo >> dispatch) league model.Player.Id
      ]
      Components.SubMenu.element (NavTo >> dispatch) (menuLinks league model)
    ]

  let view (model: Model) dispatch =
    match model.LeagueTable, model.WindowDescription with
    | Success league, Success desc -> leagueView dispatch league desc model
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueTableReceived r ->
      { model with
          LeagueTable = resultToWebData r },
      []
    | LeagueWindowDescriptionReceived r ->
      { model with
          WindowDescription = resultToWebData r },
      []
    | NavTo r -> model, navTo r
