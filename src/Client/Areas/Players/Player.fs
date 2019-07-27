namespace Areas.Players

open Elmish

open Fable.React
open Fable.React.Props

open Areas
open Shared
open Fulma
open Fable
open Routes
open Elmish.React
open Areas.Components

module Player =

  type Model =
    { Player : PlayerViewModel WebData
      PointsTotal : PredictionPointsMonoid WebData
      PlayerFixtureSets : PlayerFixtureSetsDoc option
    }

  type Msg =
    | Init of Result<string, exn>
    | NavTo of Route
    | PlayerReceived of Rresult<PlayerViewModel>
    | PlayerPointsTotalReceived of Rresult<PredictionPointsMonoid>
    | PlayerFixtureSetsReceived of Rresult<PlayerFixtureSetsDoc>

  let init api p playerId =
    { Player = Fetching
      PointsTotal = Fetching
      PlayerFixtureSets = None
    }, Cmd.batch
        [ Cmd.OfAsync.either
            (api.getPlayerInfo playerId)
            p.Token
            PlayerReceived
            (Error >> Init)
          Cmd.OfAsync.either
            (api.getPlayerPointsTotal playerId)
            p.Token
            PlayerPointsTotalReceived
            (Error >> Init)
          Cmd.OfAsync.either
            (api.getPlayerFixtureSets playerId)
            p.Token
            PlayerFixtureSetsReceived
            (Error >> Init)
        ]

  let pointsTotalView points =
    Components.card
      [ Components.pointsTotalView points
      ]

  let fixtureSetsTable dispatch (player:PlayerViewModel) (fixtureSets:PlayerFixtureSetsDoc) =
    let (PlayerId playerId) = player.Id
    Table.table [ Table.IsHoverable; Table.IsFullWidth ]
      [ thead []
          [ tr []
              [ th [] []
                th [ Class CustomClasses.TextRight ] [ str "CR" ]
                th [ Class CustomClasses.TextRight ] [ str "CS" ]
                th [ Class CustomClasses.TextRight ] [ str "Pts" ] ] ]
        tbody [] (
          fixtureSets.FixtureSets
          |> Map.toList
          |> List.sortBy (fun (_, { GameweekNo = GameweekNo gwno }) -> gwno)
          |> List.map (fun (FixtureSetId fsId, m) ->
            let (p, cr, cs) = Components.toShortPoints m.PlayerPoints
            let (GameweekNo gwno) = m.GameweekNo
            tr []
               [ td []
                    [ a [ OnClick (fun _ -> PlayerFixtureSetRoute (playerId, string fsId) |> PlayersRoute |> NavTo |> dispatch) ] [ str <| sprintf "Gameweek %i" gwno ]
                    ]
                 td [ Class CustomClasses.TextRight ] [ str (string cr) ]
                 td [ Class CustomClasses.TextRight ] [ str (string cs) ]
                 td [ Class CustomClasses.TextRight ] [ str (string p)  ]
               ]))
      ]

  let fixtureSetsView dispatch player fixtureSets =
    Card.card [ Modifiers [ Modifier.TextSize (Screen.All, TextSize.Is6) ] ]
      [ Card.content [ Props [ Style [Padding "1em 0em" ] ] ]
          [ fixtureSetsTable dispatch player fixtureSets ]
      ]

  let fullView dispatch (vm:PlayerViewModel) points fs =
    let (PlayerName name) = vm.Name
    div []
      [ Components.pageTitle name
        pointsTotalView points
        (match fs with Some f -> fixtureSetsView dispatch vm f | None -> div [] [])
      ]

  let view (model:Model) dispatch =
    match model.Player, model.PointsTotal with
    | Success player, Success points ->
      fullView dispatch player points model.PlayerFixtureSets
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | NavTo r -> model, (Routes.navTo r)
    | PlayerReceived r -> { model with Player = resultToWebData r }, []
    | PlayerPointsTotalReceived r -> { model with PointsTotal = resultToWebData r }, []
    | PlayerFixtureSetsReceived r -> { model with PlayerFixtureSets = resultToOption r }, []
