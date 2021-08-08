namespace Areas.Leagues

open Elmish

open Fable.React
open Fable.React.Props

open Shared
open Fulma
open Routes
open Areas

module PremTable =

  type PremTableType =
    | Real
    | Predicted

  type Model =
    { Type : PremTableType
      Table : PremTable WebData }

  type Msg =
    | Init
    | PremTableReceived of Rresult<PremTable>
    | NavTo of Route

  let stringToTable = function
    | "predicted" -> Predicted
    | _ -> Real

  let fetchTable (api:IProtocol) = function
    | Real -> api.getRealPremTable
    | Predicted -> api.getPredictedPremTable

  let title = function
    | Real -> Components.pageTitle "League table"
    | Predicted -> Components.pageTitle "Predicted table"

  let desc = function
    | Real -> div [] []
    | Predicted ->
            Message.message [ Message.Color IsInfo ]
              [ Message.body [ Modifiers [ Modifier.TextAlignment (Screen.Mobile, TextAlignment.Left) ] ]
                  [ str "How the table would look if all your predictions were correct" ]
              ]


  let init api player table =
    { Type = table |> stringToTable; Table = Fetching },
      Cmd.OfAsync.perform
        (table |> stringToTable |> fetchTable api)
        player.Token
        PremTableReceived

  let buildTable table =
    Table.table [ Table.IsHoverable; Table.IsFullWidth ]
      [ thead []
          [ tr []
              [ th [] []
                th [] []
                th [ Class CustomClasses.TextRight ] [ str "P" ]
                th [ Class CustomClasses.TextRight ] [ str "GD" ]
                th [ Class CustomClasses.TextRight ] [ str "Pts" ] ] ]
        tbody [] (
          table.Rows
          |> Map.toList
          |> List.sortBy (fun (_, m) -> m.Position)
          |> List.map (fun (Team team, m) ->
            tr [ ]
               [ td [ Class CustomClasses.TextRight ] [ str (string m.Position) ]
                 td [] [ str team ]
                 td [ Class CustomClasses.TextRight ] [ str (string m.Played) ]
                 td [ Class CustomClasses.TextRight ] [ str (m.GoalsFor - m.GoalsAgainst |> Components.signedInt) ]
                 td [ Class CustomClasses.TextRight ] [ str (string m.Points) ]
               ]
          ))
      ]

  let view (model:Model) dispatch =
    match model.Table with
    | Success table ->
      div []
        [ title model.Type
          desc model.Type
          buildTable table ]
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | PremTableReceived r ->
      { model with Table = resultToWebData r }, []
    | NavTo r ->
      model, Routes.navTo r