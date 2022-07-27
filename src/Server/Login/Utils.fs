namespace Server.Utils

open System
open Microsoft.AspNetCore.Http

module Urls =

  let buildUrl baseUrl =
    List.map (fun (k, v) -> sprintf "%s=%s" k v)
    >> fun p -> String.Join("&", p)
    >> fun q -> sprintf "%s?%s" baseUrl q

module HttpContext =

  // let baseUrl (ctx:HttpContext) =
  //   sprintf "%s://%s" ctx.Request.Scheme (ctx.Request.Host.ToString())

  // let escapedPathOnBaseUrl (ctx:HttpContext) path =
  //   sprintf "%s%s" (baseUrl ctx) path
  //   |> Uri.EscapeUriString

  let escapedPath baseUrl path =
    sprintf "%s%s" baseUrl path
    |> Uri.EscapeDataString

  let requestFormKey (ctx:HttpContext) key =
    match ctx.Request.Form.TryGetValue(key) with
    | true, value -> value.ToString()
    | _ -> ""