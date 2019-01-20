namespace Areas.Leagues

open Elmish

open Fable.Helpers.React
open Fable.Helpers.React.Props

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
    }, Cmd.batch
        ([ Cmd.ofAsync
            (api.getLeagueTable leagueId window)
            player.Token
            LeagueTableReceived
            (Error >> Init)
        ] @
        match window with
        | Full -> []
        | Week _ -> []
        | Month (y, m) ->
          [ Cmd.ofAsync
              (api.getDateFormat (DateTime(y, m, 1)) "MMMM yyyy")
              player.Token
              LeagueWindowDescriptionReceived
              (Error >> Init)
          ]
        )

  let leagueView (league:LeagueTableDoc) desc model dispatch =
    let playerClick (PlayerId pId) =
      pId |> (PlayerRoute >> PlayersRoute >> NavTo >> dispatch)
    let (LeagueName name) =
      league.LeagueName
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Components.subHeading desc
        Card.card []
          [ Components.table league playerClick
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
