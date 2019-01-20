namespace Areas.Leagues

open Elmish
open Shared
open Routes
open System

module LeaguesArea =

  type Model =
    | CreateLeagueModel of CreateLeague.Model
    | GlobalLeagueModel of GlobalLeague.Model
    | JoinLeagueModel of JoinLeague.Model
    | LeaveLeagueModel of LeaveLeague.Model
    | LeagueModel of League.Model
    | LeaguesModel of Leagues.Model
    | LeagueHistoryModel of LeagueHistory.Model
    | LeagueHistoryTableModel of LeagueHistoryTable.Model
    | LeagueMatrixModel of LeagueMatrix.Model
    | LeagueTableModel of LeagueTable.Model

  type Msg =
    | CreateLeagueMsg of CreateLeague.Msg
    | GlobalLeagueMsg of GlobalLeague.Msg
    | JoinLeagueMsg of JoinLeague.Msg
    | LeaveLeagueMsg of LeaveLeague.Msg
    | LeagueMsg of League.Msg
    | LeaguesMsg of Leagues.Msg
    | LeagueHistoryMsg of LeagueHistory.Msg
    | LeagueHistoryTableMsg of LeagueHistoryTable.Msg
    | LeagueMatrixMsg of LeagueMatrix.Msg
    | LeagueTableMsg of LeagueTable.Msg

  let update api p message model =
    match message, model with
    | CreateLeagueMsg msg, CreateLeagueModel m ->
      CreateLeague.update api p msg m |> fun (m, cmd) -> CreateLeagueModel m, Cmd.map CreateLeagueMsg cmd
    | GlobalLeagueMsg msg, GlobalLeagueModel m ->
      GlobalLeague.update api p msg m |> fun (m, cmd) -> GlobalLeagueModel m, Cmd.map GlobalLeagueMsg cmd
    | JoinLeagueMsg msg, JoinLeagueModel m ->
      JoinLeague.update api p msg m |> fun (m, cmd) -> JoinLeagueModel m, Cmd.map JoinLeagueMsg cmd
    | LeaveLeagueMsg msg, LeaveLeagueModel m ->
      LeaveLeague.update api p msg m |> fun (m, cmd) -> LeaveLeagueModel m, Cmd.map LeaveLeagueMsg cmd
    | LeagueMsg msg, LeagueModel m ->
      League.update api p msg m |> fun (m, cmd) -> LeagueModel m, Cmd.map LeagueMsg cmd
    | LeaguesMsg msg, LeaguesModel m ->
      Leagues.update api p msg m |> fun (m, cmd) -> LeaguesModel m, Cmd.map LeaguesMsg cmd
    | LeagueHistoryMsg msg, LeagueHistoryModel m ->
      LeagueHistory.update api p msg m |> fun (m, cmd) -> LeagueHistoryModel m, Cmd.map LeagueHistoryMsg cmd
    | LeagueHistoryTableMsg msg, LeagueHistoryTableModel m ->
      LeagueHistoryTable.update api p msg m |> fun (m, cmd) -> LeagueHistoryTableModel m, Cmd.map LeagueHistoryTableMsg cmd
    | LeagueMatrixMsg msg, LeagueMatrixModel m ->
      LeagueMatrix.update api p msg m |> fun (m, cmd) -> LeagueMatrixModel m, Cmd.map LeagueMatrixMsg cmd
    | LeagueTableMsg msg, LeagueTableModel m ->
      LeagueTable.update api p msg m |> fun (m, cmd) -> LeagueTableModel m, Cmd.map LeagueTableMsg cmd
    | _ -> model, alert (LoginProblem "league msg not matched")

  let urlUpdate api p = function
    | CreateLeagueRoute ->
      CreateLeague.init api p |> fun (m, cmd) -> CreateLeagueModel m, Cmd.map CreateLeagueMsg cmd
    | GlobalLeagueRoute ->
      GlobalLeague.init api p |> fun (m, cmd) -> GlobalLeagueModel m, Cmd.map GlobalLeagueMsg cmd
    | JoinLeagueRoute leagueId when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> JoinLeague.init api p |> fun (m, cmd) -> JoinLeagueModel m, Cmd.map JoinLeagueMsg cmd
    | LeaveLeagueRoute leagueId when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> LeaveLeague.init api p |> fun (m, cmd) -> LeaveLeagueModel m, Cmd.map LeaveLeagueMsg cmd
    | LeagueRoute leagueId when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> League.init api p |> fun (m, cmd) -> LeagueModel m, Cmd.map LeagueMsg cmd
    | LeagueHistoryRoute leagueId when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> PrivateLeague |> LeagueHistory.init api p |> fun (m, cmd) -> LeagueHistoryModel m, Cmd.map LeagueHistoryMsg cmd
    | LeagueHistoryRoute leagueId when leagueId = Global.identifier ->
      GlobalLeague |> LeagueHistory.init api p |> fun (m, cmd) -> LeagueHistoryModel m, Cmd.map LeagueHistoryMsg cmd
    | LeagueHistoryFixtureSetRoute (leagueId, gw) when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> PrivateLeague |> LeagueHistoryTable.init api p (Week gw) |> fun (m, cmd) -> LeagueHistoryTableModel m, Cmd.map LeagueHistoryTableMsg cmd
    | LeagueHistoryFixtureSetRoute (leagueId, gw) when leagueId = Global.identifier ->
      GlobalLeague |> LeagueHistoryTable.init api p (Week gw) |> fun (m, cmd) -> LeagueHistoryTableModel m, Cmd.map LeagueHistoryTableMsg cmd

    | LeagueHistoryMonthRoute (leagueId, year, month) when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> PrivateLeague |> LeagueHistoryTable.init api p (Month (year, month)) |> fun (m, cmd) -> LeagueHistoryTableModel m, Cmd.map LeagueHistoryTableMsg cmd
    | LeagueHistoryMonthRoute (leagueId, year, month) when leagueId = Global.identifier ->
      GlobalLeague |> LeagueHistoryTable.init api p (Month (year, month)) |> fun (m, cmd) -> LeagueHistoryTableModel m, Cmd.map LeagueHistoryTableMsg cmd

    | LeagueMatrixRoute (leagueId, gwno) when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> PrivateLeague |> LeagueMatrix.init api p (GameweekNo gwno) |> fun (m, cmd) -> LeagueMatrixModel m, Cmd.map LeagueMatrixMsg cmd
    | LeagueMatrixRoute (leagueId, gwno) when leagueId = Global.identifier ->
      GlobalLeague |> LeagueMatrix.init api p (GameweekNo gwno) |> fun (m, cmd) -> LeagueMatrixModel m, Cmd.map LeagueMatrixMsg cmd

    | LeagueTableRoute leagueId when isValidGuid leagueId ->
      toGuid leagueId |> PrivateLeagueId |> PrivateLeague |> LeagueTable.init api p |> fun (m, cmd) -> LeagueTableModel m, Cmd.map LeagueTableMsg cmd
    | LeagueTableRoute leagueId when leagueId = Global.identifier ->
      GlobalLeague |> LeagueTable.init api p |> fun (m, cmd) -> LeagueTableModel m, Cmd.map LeagueTableMsg cmd

    | PlayerLeaguesRoute
    | _ ->
      Leagues.init api p |> fun (m, cmd) -> LeaguesModel m, Cmd.map LeaguesMsg cmd

  let view model dispatch =
    match model with
    | CreateLeagueModel m ->
      CreateLeague.view m (CreateLeagueMsg >> dispatch)
    | GlobalLeagueModel m ->
      GlobalLeague.view m (GlobalLeagueMsg >> dispatch)
    | JoinLeagueModel m ->
      JoinLeague.view m (JoinLeagueMsg >> dispatch)
    | LeaveLeagueModel m ->
      LeaveLeague.view m (LeaveLeagueMsg >> dispatch)
    | LeagueModel m ->
      League.view m (LeagueMsg >> dispatch)
    | LeaguesModel m ->
      Leagues.view m (LeaguesMsg >> dispatch)
    | LeagueHistoryModel m ->
      LeagueHistory.view m (LeagueHistoryMsg >> dispatch)
    | LeagueHistoryTableModel m ->
      LeagueHistoryTable.view m (LeagueHistoryTableMsg >> dispatch)
    | LeagueMatrixModel m ->
      LeagueMatrix.view m (LeagueMatrixMsg >> dispatch)
    | LeagueTableModel m ->
      LeagueTable.view m (LeagueTableMsg >> dispatch)
