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
        | Full -> NotAsked
        | Week w -> sprintf "Gameweek %i" w |> Success
        | Month _ -> Fetching
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
        | Full -> []
        | Week _ -> []
        | Month (y, m) ->
          [ Cmd.OfAsync.either
              /// use (y, m, 2) to prevent against utc vs gmt+0100 issues
              (api.getDateFormat (DateTime(y, m, 2)) "MMMM yyyy")
              player.Token
              LeagueWindowDescriptionReceived
              (Error >> Init)
          ]
        )

  let leagueView (league:LeagueTableDoc) desc (model:Model) dispatch =
    let playerClick (PlayerId pId) =
      pId |> (PlayerRoute >> PlayersRoute >> NavTo >> dispatch)
    let (LeagueName name) =
      league.LeagueName
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Components.subHeading desc
        Card.card []
          [ Components.table league model.Player.Id playerClick
          ]
      ]

  let view (model:Model) dispatch =
    match model.LeagueTable, model.WindowDescription with
    | Success league, Success desc -> leagueView league desc model dispatch
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueTableReceived r -> { model with LeagueTable = resultToWebData r }, []
    | LeagueWindowDescriptionReceived r -> { model with WindowDescription = resultToWebData r }, []
    | NavTo r -> model, navTo r
