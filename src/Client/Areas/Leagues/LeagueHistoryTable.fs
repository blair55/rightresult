namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open System
open Shared
open Fulma
open Routes
open Areas

module LeagueHistoryTable =

  type Model =
    { Window : LeagueWindow
      WindowDescription : string WebData
      LeagueTable : LeagueTableDoc WebData
      Player : ClientSafePlayer
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueTableReceived of Rresult<LeagueTableDoc>
    | LeagueWindowDescriptionReceived of Rresult<string>
    | NavTo of Route

  let init api player window leagueId =
    { Window = window
      WindowDescription =
        match window with
        | Week w -> sprintf "Gameweek %i" w |> Success
        | Month _ -> Fetching
        | _ -> NotAsked
      LeagueTable = Fetching
      Player = player
    }, Cmd.batch
        ([ Cmd.OfAsync.either
            (api.getLeagueTable leagueId window)
            player.Token
            LeagueTableReceived
            (Error >> Init)
        ] @
        match window with
        | Month (y, m) ->
          [ Cmd.OfAsync.either
              /// use (y, m, 2) to prevent against utc vs gmt+0100 issue
              (api.getDateFormat (DateTime(y, m, 2)) "MMMM yyyy")
              player.Token
              LeagueWindowDescriptionReceived
              (Error >> Init)
          ]
        | _ -> []
        )

  let leagueView dispatch (league:LeagueTableDoc) desc (model:Model) =
    let (LeagueName name) = league.LeagueName
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Components.subHeading desc
        Card.card []
          [ Components.table (NavTo >> dispatch) league model.Player.Id
          ]
      ]

  let view (model:Model) dispatch =
    match model.LeagueTable, model.WindowDescription with
    | Success league, Success desc -> leagueView dispatch league desc model
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueTableReceived r -> { model with LeagueTable = resultToWebData r }, []
    | LeagueWindowDescriptionReceived r -> { model with WindowDescription = resultToWebData r }, []
    | NavTo r -> model, navTo r
