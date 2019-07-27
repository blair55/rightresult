namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Areas
open Shared
open Fulma
open Routes

module LeagueTable =

  type Model =
    { LeagueId : LeagueId
      League : LeagueTableDoc WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | NavTo of Route

  let init api player leagueId =
    { LeagueId = leagueId
      League = Fetching
    },
      Cmd.OfAsync.either
        (api.getLeagueTable leagueId Full)
        player.Token
        LeagueReceived
        (Error >> Init)

  let leagueView (league:LeagueTableDoc) (model:Model) dispatch =
    let playerClick (PlayerId pId) =
      pId |> (PlayerRoute >> PlayersRoute >> NavTo >> dispatch)
    let (LeagueName name) =
      league.LeagueName
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Card.card []
          [ Components.table league playerClick
          ]
      ]

  let view (model:Model) dispatch =
    match model.League with
    | NotAsked
    | Fetching -> div [] []
    | WebError _ -> div [] [ str "could not find league" ]
    | Success league -> leagueView league model dispatch

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | NavTo r -> model, navTo r
