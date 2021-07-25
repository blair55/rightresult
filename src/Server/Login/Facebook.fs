namespace Server.Login

open Giraffe
open FSharp.Data
open FSharp.Data.JsonExtensions
open Microsoft.AspNetCore.Http
open Server.Elevated
open Server.Utils
open Shared
// open Shared.Routes

module Facebook =

  let private apiVersion = "v3.3"
  let private callbackPath = "/api/facebook/callback"

  type Url = Dialog | Oauth | Graph
  type QueryString = (string * string) list
  type UrlQuery = UrlQuery of Url * QueryString
  type Code = Code of string
  type AccessTokenJson = AccessTokenJson of string
  type AccessToken = AccessToken of string
  type GraphQueryResultJson = GraphQueryResultJson of string

  type Configuration =
    { clientId : string
      clientSecret : string
      baseUrl : string
      middleWare : HttpHandler
      authOk : ExternalAuth -> HttpHandler
      authError : string -> HttpHandler }

  type AuthError =
    | NoCodeReturned
    | BadResponseFromOauth of string
    | NoAccessTokenReturned
    | BadResponseFromGraph of string
    | CouldNotBuildGraphResult

  let private urlQueryToString (UrlQuery (u, qs)) =
    match u with
    | Dialog -> sprintf "https://www.facebook.com/%s/dialog/oauth" apiVersion
    | Oauth  -> sprintf "https://graph.facebook.com/%s/oauth/access_token" apiVersion
    | Graph  -> sprintf "https://graph.facebook.com/%s/me" apiVersion
    |> fun urlPath -> Urls.buildUrl urlPath qs

  let private requestUrl uq =
    printfn "requesting url %s" (urlQueryToString uq)
    Http.AsyncRequestString(urlQueryToString uq, silentHttpErrors=true)

  let private getCode (ctx:HttpContext) =
    match ctx.TryGetQueryStringValue "code" with
    | None -> Error NoCodeReturned
    | Some code -> Ok (Code code)

  let private getAccessTokenJson (ctx:HttpContext) (config:Configuration) (Code code) =
    try
      (Oauth,
        [ "client_id", config.clientId
          "client_secret", config.clientSecret
          "redirect_uri", HttpContext.escapedPath config.baseUrl callbackPath
          "code", code ])
      |> UrlQuery
      |> requestUrl
      |> Async.map (AccessTokenJson >> Ok)
    with _ ->
      printfn "bad e"
      Async.retn (BadResponseFromOauth "" |> Error)

  let private getAccessToken (AccessTokenJson json) =
    try
      (JsonValue.Parse json)?access_token.AsString()
      |> AccessToken
      |> Ok
    with _ ->
      Error NoAccessTokenReturned

  let private queryGraph (AccessToken token) =
    try
      (Graph,
        [ "access_token", token ])
      |> UrlQuery
      |> requestUrl
      |> Async.map (GraphQueryResultJson >> Ok)
    with _ ->
      printfn "bad graph"
      Async.retn (BadResponseFromGraph "" |> Error)

  let private buildExtAuth (GraphQueryResultJson json) =
    try
      printfn "%s" json
      let j = JsonValue.Parse json
      { id = j?id.AsString()
        name = j?name.AsString()
        email =
          j.TryGetProperty("email")
          |> function | Some e -> e.AsString() | None -> "" }
      |> Ok
    with _ ->
      printfn "bad json"
      Error CouldNotBuildGraphResult

  let private callbackHandler (config:Configuration) : HttpHandler =
    fun next ctx ->
      let respond (result:Result<ExternalAuth, AuthError>) =
        match result with
        | Ok gqr ->
          config.authOk gqr next ctx
        | Error e ->
          config.authError (sprintf "%A" e) next ctx
      ctx
      |> (getCode
      >> Async.retn
      >> AsyncResult.bind (getAccessTokenJson ctx config)
      >> Async.map (Result.bind getAccessToken)
      >> AsyncResult.bind queryGraph
      >> Async.toTask (Async.map (Result.bind buildExtAuth))
      >> Task.bind respond)

  let private iniateRedirectHandler (config:Configuration) : HttpHandler =
    fun next ctx ->
      // ctx.Response.Cookies.Append(redirectPathKey, HttpContext.requestFormKey ctx redirectPathKey)
      (Dialog,
        [ "client_id", config.clientId
          "redirect_uri", HttpContext.escapedPath config.baseUrl callbackPath
          "state", ""
          "response_type", "code"
          "scope", "email" ])
      |> UrlQuery
      |> urlQueryToString
      |> fun url -> redirectTo false url next ctx

  let handler config : HttpHandler =
    choose [
      POST >=> route Routes.redirectToFacebookPath >=> config.middleWare >=> iniateRedirectHandler config
      GET >=> route callbackPath >=> callbackHandler config ]
