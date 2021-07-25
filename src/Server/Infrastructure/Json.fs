module Server.Infrastructure.Json

open System
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let private jsonConverter =
  Fable.JsonConverter() :> JsonConverter

let srlzToString body =
  JsonConvert.SerializeObject(body, [|jsonConverter|])

let srlz f =
  srlzToString f |> Encoding.UTF8.GetBytes |> ReadOnlyMemory

// let dsrlz<'a> (bytes:byte array) =
let dsrlz<'a> (bytes:ReadOnlyMemory<byte>) =
  let s = Encoding.UTF8.GetString (bytes.ToArray())
  // printfn "type: %s / string: %s" typeof<'a>.Name s
  JsonConvert.DeserializeObject<'a> (s, [|jsonConverter|])

let dsrlzStrng<'a> (s:string) =
  // printfn "type: %s / string: %s" typeof<'a>.Name s
  JsonConvert.DeserializeObject<'a> (s, [|jsonConverter|])

let dsrlzStrngAtPath<'a> path (s:string) =
  JObject.Parse(s).[path].ToString() |> dsrlzStrng<'a>

let dsrlzStrngAtPath2<'a> path1 path2 (s:string) =
  JObject.Parse(s).[path1].[path2].ToString() |> dsrlzStrng<'a>
