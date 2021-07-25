namespace Server

open System
open Newtonsoft.Json
open Shared

module Jwt =
// how to generate secret string

(*
let crypto = System.Security.Cryptography.RandomNumberGenerator.Create();;
let randomNumber = Array.init 32 byte;;
crypto.GetBytes(randomNumber);;
System.Convert.ToBase64String(randomNumber);;
*)

  type JwtPlayer = {
    sub : string
    name : string
    playerId : string
    roles : string list }

  type EncryptionSecretBase64 =
    EncryptionSecretBase64 of string

  let jwtPlayerToAppToken (EncryptionSecretBase64 secret) (player : JwtPlayer) : AppToken =
    Jose.JWT.Encode(
      JsonConvert.SerializeObject player,
      System.Convert.FromBase64String secret,
      Jose.JweAlgorithm.A256KW,
      Jose.JweEncryption.A256CBC_HS512)
    |> AppToken

  let appTokenToJwtPlayer (EncryptionSecretBase64 secret) (AppToken token) : JwtPlayer =
    Jose.JWT.Decode(
      token,
      System.Convert.FromBase64String secret,
      Jose.JweAlgorithm.A256KW,
      Jose.JweEncryption.A256CBC_HS512)
    |> JsonConvert.DeserializeObject<JwtPlayer>
