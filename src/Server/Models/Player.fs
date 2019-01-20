namespace Server

open Shared
open Server.Commands
open Server.Events

module Player =

  type PlayerState =
    | Init
    | Active of PlayerId

  let private fold player event =
    match player, event with
    | Init, PlayerCreated (playerId, _, _) -> Active playerId |> Ok
    | Active playerId, PlayerLoggedIn _ -> Active playerId |> Ok
    | Active _, PlayerRemoved _ -> Init |> Ok
    | _ -> HandlerHelper.eventErr event player

  let folder =
    fold, Init

  let apply (playerId, command) player : Rresult<Event list> =
    match player, command with
    | Init, Login (playerName, email) ->
      [ PlayerCreated (playerId, playerName, email)
        PlayerLoggedIn playerId ] |> Ok
    | Active playerId, Login _ ->
      [ PlayerLoggedIn playerId ] |> Ok
    | Active playerId, Remove ->
      [ PlayerRemoved playerId ] |> Ok
    | _ -> HandlerHelper.cmdErr command player
