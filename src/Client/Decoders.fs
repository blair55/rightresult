module Decoders

open Shared
open Thoth.Json


let decodeAppToken =
  Decode.object (fun get -> get.Required.Field "AppToken" Decode.string |> AppToken)

let decodePlayerId =
  Decode.object (fun get -> get.Required.Field "PlayerId" Decode.string |> PlayerId)

let decodeClientSafePlayer : Decode.Decoder<ClientSafePlayer> =
  Decode.object
    (fun get ->
      { Name = get.Required.Field "Name" Decode.string
        Id = get.Required.Field "Id" decodePlayerId
        Token = get.Required.Field "Token" decodeAppToken })
