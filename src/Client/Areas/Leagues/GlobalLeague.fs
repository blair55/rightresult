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
      MaxGameweekNo : GameweekNo WebData
    }

  type Msg =
    | Init of Result<string, exn>
    | LeagueReceived of Rresult<LeagueTableDoc>
    | MaxGwnoReceived of Rresult<GameweekNo>
    | NavTo of Route

  let init api player =
    Cmd.batch
      [ Cmd.OfAsync.either
          (api.getLeagueTable GlobalLeague Full)
          player.Token
          LeagueReceived
          (Error >> Init)
        Cmd.OfAsync.either
          api.getMaxGameweekNo
          player.Token
          MaxGwnoReceived
          (Error >> Init)
      ]
    |> fun cmds ->
    { League = Fetching
      MaxGameweekNo = Fetching
    }, cmds

  let leagueView {LeagueTableDoc.LeagueName = LeagueName name} (GameweekNo gwno) dispatch =
    let footer =
      Components.leagueMenu Global.identifier gwno (NavTo >> dispatch)
    div [ ClassName "block" ]
      [ Components.pageTitle name
        Message.message [ Message.Color IsWarning ]
          [ Message.body [ ]
              [ str "To take part in prize money league contact"
                str " "
                a [ Href "mailto:predictionleague1@hotmail.com"] [ str "predictionleague1@hotmail.com" ]
                br []
                str "Check the Leagues tab to confirm which leagues you have entered" ] ]
        Components.subHeading "Standings"
        Card.card [ CustomClass "card-footer-only" ]
          [ div [] [ footer ]
          ]
      ]

  let view (model:Model) dispatch =
    match model.League, model.MaxGameweekNo with
    | Success league, Success gwno -> leagueView league gwno dispatch
    | _ -> div [] [ str "could not find league" ]

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | LeagueReceived r -> { model with League = resultToWebData r }, []
    | MaxGwnoReceived r -> { model with MaxGameweekNo = resultToWebData r }, []
    | NavTo r -> model, navTo r
