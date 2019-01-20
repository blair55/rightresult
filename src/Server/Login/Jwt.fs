namespace Server

open System
open Newtonsoft.Json
open Shared

module Jwt =
// how to generate secret string

// > let crypto = System.Security.Cryptography.RandomNumberGenerator.Create();;
// val crypto : System.Security.Cryptography.RandomNumberGenerator

// > let randomNumber = Array.init 32 byte;;
// val randomNumber : byte [] =
//   [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy; 11uy; 12uy; 13uy;
//     14uy; 15uy; 16uy; 17uy; 18uy; 19uy; 20uy; 21uy; 22uy; 23uy; 24uy; 25uy;
//     26uy; 27uy; 28uy; 29uy; 30uy; 31uy|]

// > crypto.GetBytes(randomNumber);;
// val it : unit = ()

// > System.Convert.ToBase64String(randomNumber);;
// val it : string = "CFOBCC7wuZT7Wdzy+pwn/x2xPid4GZbT/p1bTfu4t5I="

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
