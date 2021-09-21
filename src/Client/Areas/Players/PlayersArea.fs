namespace Areas.Players

open Elmish
open Shared
open Routes

module PlayersArea =

  type Model =
    | MyProfileModel of MyProfile.Model
    | PlayerModel of Player.Model
    | AllPlayersModel of AllPlayers.Model
    | PlayerGameweekModel of PlayerGameweek.Model

  type Msg =
    | MyProfileMsg of MyProfile.Msg
    | PlayerMsg of Player.Msg
    | AllPlayersMsg of AllPlayers.Msg
    | PlayerGameweekMsg of PlayerGameweek.Msg

  let update api p message model =
    match message, model with
    | MyProfileMsg msg, MyProfileModel m ->
      MyProfile.update api p msg m |> fun (m, cmd) -> MyProfileModel  m, Cmd.map MyProfileMsg cmd
    | PlayerMsg msg, PlayerModel m ->
      Player.update api p msg m |> fun (m, cmd) -> PlayerModel m, Cmd.map PlayerMsg cmd
    | AllPlayersMsg msg, AllPlayersModel m ->
      AllPlayers.update api p msg m |> fun (m, cmd) -> AllPlayersModel m, Cmd.map AllPlayersMsg cmd
    | PlayerGameweekMsg msg, PlayerGameweekModel m ->
      PlayerGameweek.update api p msg m |> fun (m, cmd) -> PlayerGameweekModel m, Cmd.map PlayerGameweekMsg cmd
    | _ -> model, alert (LoginProblem "player msg not matched")

  let urlUpdate api p = function
    | MyProfileRoute ->
      MyProfile.init api p |> fun (m, cmd) -> MyProfileModel m, Cmd.map MyProfileMsg cmd
    | PlayerRoute playerId ->
      PlayerId playerId |> Player.init api p |> fun (m, cmd) -> PlayerModel m, Cmd.map PlayerMsg cmd
    | AllPlayersRoute ->
      AllPlayers.init api p |> fun (m, cmd) -> AllPlayersModel m, Cmd.map AllPlayersMsg cmd
    | PlayerGameweekRoute (playerId, gwno) ->
      PlayerGameweek.init api p (PlayerId playerId) (GameweekNo gwno) |> fun (m, cmd) -> PlayerGameweekModel m, Cmd.map PlayerGameweekMsg cmd

  let view model dispatch =
    match model with
    | MyProfileModel m ->
      MyProfile.view m (MyProfileMsg >> dispatch)
    | PlayerModel m ->
      Player.view m (PlayerMsg >> dispatch)
    | AllPlayersModel m ->
      AllPlayers.view m (AllPlayersMsg >> dispatch)
    | PlayerGameweekModel m ->
      PlayerGameweek.view m (PlayerGameweekMsg >> dispatch)
