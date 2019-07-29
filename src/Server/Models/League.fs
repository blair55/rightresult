namespace Server

open Shared
open Server.Commands
open Server.Events

module League =

  type LeagueState =
    | Init
    | Active of Set<PlayerId>
    | Removed

  let private fold league event =
    match league, event with
    | Init, LeagueCreated _ ->
      Active Set.empty |> Ok
    | Active players, LeagueRenamed _ ->
      Active players |> Ok
    | Active players, LeagueJoined (_, pId) ->
      Active (players.Add pId) |> Ok
    | Active players, LeagueLeft (_, pId) ->
      Active (players.Remove pId) |> Ok
    | Active _, LeagueRemoved _ ->
      Ok Removed
    | _ -> HandlerHelper.eventErr event league

  let folder =
    fold, Init


  let apply (leagueId, command) league : Rresult<Event list> =
    match league, command with
    | Init, CreateLeague (playerId, leagueName) ->
      [ LeagueCreated (leagueId, leagueName, playerId)
        LeagueJoined (leagueId, playerId) ] |> Ok
    | Active _, RenameLeague leagueName ->
      Ok [ LeagueRenamed (leagueId, leagueName) ]
    | Active players, JoinLeague playerId ->
      if players.Contains playerId
      then Ok []
      else [ LeagueJoined (leagueId, playerId) ] |> Ok
    | Active players, LeaveLeague playerId ->
      if players.Contains playerId
      then [ LeagueLeft (leagueId, playerId) ] |> Ok
      else Ok []
    | Active _, RemoveLeague _ ->
      [ LeagueRemoved leagueId ] |> Ok
    | _ -> HandlerHelper.cmdErr command league
