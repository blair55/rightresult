namespace Areas.Components

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared
open Fulma
open Routes

module GameweekStats =

  let private pointsHeading (player: ClientSafePlayer) (stats: GlobalGameweekStats) =
    // if player.Id = stats.PlayerId then
    //   str "my points"
    // else
    //   str "points"
    str "pts"

  let private maxPointsPlayerGwRoute (PlayerId playerId) (GameweekNo gwno) =
    PlayerGameweekRoute(playerId, gwno)
    |> PlayersRoute

  let element nav (player: ClientSafePlayer) (stats: GlobalGameweekStats) =
    div [ Class "gw-stats" ] [
      Level.level [ Level.Level.Option.IsMobile ] [
        Level.item [ Level.Item.HasTextCentered ] [
          div [] [
            Level.heading [] [
              pointsHeading player stats
            ]
            Level.title [] [
              str (
                match stats.Player with
                | Some (p, _, _) -> string p
                | _ -> "-"
              )
            ]
          ]
        ]
        Level.item [ Level.Item.HasTextCentered ] [
          div [] [
            Level.heading [] [ str "avg" ]
            Level.title [] [
              str (string (stats.AveragePoints))
            ]
          ]
        ]
        Level.item [ Level.Item.HasTextCentered ] [
          div [] [
            Level.heading [] [ str "max" ]
            Level.title
              []
              (match stats.MaximumPoints with
               | Some (maxPlayerId, maxPoints) ->
                 [ (a
                     (maxPointsPlayerGwRoute maxPlayerId stats.GameweekNo
                      |> Areas.Components.anchorNavProps nav)
                     [ str (string maxPoints) ]) ]
               | None _ -> [ span [] [ str "-" ] ])
          ]
        ]
        Level.item [ Level.Item.HasTextCentered ] [
          div [] [
            Level.heading [] [ str " pos" ]
            Level.title
              []
              (match stats.Player with
               | Some (_, p, ord) ->
                 [ span [] [ str (string p) ]
                   span [ Class "ordinal" ] [ str (ord) ] ]
               | _ -> [ span [] [ str "-" ] ])
          ]
        ]
      ]
    ]
