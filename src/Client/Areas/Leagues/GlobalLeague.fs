namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props
open Fable.Import

open Shared
open Areas
open Fulma
open Fable
open Routes
open Elmish.ReactNative

module GlobalLeague =

  type Model =
    { League : LeagueTableDoc WebData
      ActiveGameweekNo : GameweekNo WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | ActiveGwnoReceived of Rresult<GameweekNo>
    | NavTo of Route

  let init api player =
    Cmd.batch
      [ Cmd.OfAsync.either
          (api.getLeagueTable GlobalLeague Full)
          player.Token
          LeagueReceived
          (Error >> Init)
        Cmd.OfAsync.either
          api.getEarliestOpenGwno
          player.Token
          ActiveGwnoReceived
          (Error >> Init)
      ]
    |> fun cmds ->
    { League = Fetching
      ActiveGameweekNo = Fetching
    }, cmds

  let leagueView {LeagueTableDoc.LeagueName = LeagueName name} (GameweekNo gwno) dispatch =
    let footer =
      Components.leagueMenu Global.identifier gwno (NavTo >> dispatch)
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Components.subHeading "Standings"
        Card.card [ CustomClass "card-footer-only" ]
          [ div [] [ footer ]
          ]
      ]

  let view (model:Model) dispatch =
    match model.League, model.ActiveGameweekNo with
    | Success league, Success gwno -> leagueView league gwno dispatch
    | WebError _, _
    | _, WebError _ -> div [] [ str "could not find league" ]
    | _ -> div [] [  ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | ActiveGwnoReceived r -> { model with ActiveGameweekNo = resultToWebData r }, []
    | NavTo r -> model, navTo r
