namespace Server.Login

open System
open System.Security.Cryptography
open Giraffe
open FSharp.Data
open Server.Utils

open Shared
// open Shared.Routes
open System.Text
open System.Net
open System.Web

module Twitter =

  type Configuration =
    { consumerKey : string
      consumerSecret : string
      baseUrl : string
      middleWare : HttpHandler
      authOk : ExternalAuth -> HttpHandler
      authError : string -> HttpHandler }

  let private callbackPath = "/api/twitter/callback"

  let urlEncode = Uri.EscapeDataString
  let b64Encode = Convert.ToBase64String
  let prependOAuthLead = sprintf "OAuth %s"
  let quoteWrap = sprintf "\"%s\""
  let merge (k, v) = sprintf "%s=%s" k v
  let commaJoin (a:string seq) = String.Join(", ", a)
  let ampersandJoin (a:string seq) = String.Join("&", a)
  let toBytes (s:string) = Encoding.ASCII.GetBytes s
  let parseQs =
    HttpUtility.ParseQueryString
    >> fun nvc -> nvc.AllKeys |> Seq.map (fun k -> k, nvc.[k]) |> Map.ofSeq

  let iniateRedirectHandler (config:Configuration) next ctx =
    let method = "POST"
    let requestTokenUrl = "https://api.twitter.com/oauth/request_token"
    let authenticateUrl = "https://api.twitter.com/oauth/authenticate"
    let callbackUrl = sprintf "%s%s" config.baseUrl callbackPath

    // ctx.Response.Cookies.Append(redirectPathKey, HttpContext.requestFormKey ctx redirectPathKey)
    let nonce =
      Guid.NewGuid().ToString("N")

    let timestamp =
      DateTimeOffset.UtcNow.ToUnixTimeSeconds().ToString()

    let auth callbackUrl =
      [ "oauth_callback", callbackUrl
        "oauth_consumer_key", config.consumerKey
        "oauth_nonce", nonce
        "oauth_signature_method", "HMAC-SHA1"
        "oauth_timestamp", timestamp
        "oauth_version", "1.0" ]

    let oAuthParamsForSigning =
      auth callbackUrl
      |> List.map (fun (k, v) -> k, urlEncode v)
      |> List.sortBy (fun (k, _) -> k)
      |> List.map merge

    let paramsForSigning =
      [ method
        urlEncode requestTokenUrl
        ampersandJoin oAuthParamsForSigning |> urlEncode ]
      |> ampersandJoin

    let signingKey =
      [ urlEncode config.consumerSecret; "" ]
      |> ampersandJoin

    use hmac = new HMACSHA1(toBytes signingKey)
    let signature = toBytes paramsForSigning |> hmac.ComputeHash |> b64Encode

    let oAuthParamsForSending =
      ("oauth_signature", signature) :: auth callbackUrl
      |> List.map (fun (k, v) -> k, urlEncode v |> quoteWrap)
      |> List.sortBy (fun (k, _) -> k)
      |> List.map merge
      |> commaJoin
      |> prependOAuthLead

    let headers =
      [ "Authorization", oAuthParamsForSending ]

    let r = Http.RequestString(requestTokenUrl, headers=headers, httpMethod=method, silentHttpErrors=true)
    printfn "%s" r
    let qs = parseQs r
    let token = qs.["oauth_token"]
    let redirectUrl = sprintf "%s?oauth_token=%s" authenticateUrl token
    redirectTo false redirectUrl next ctx

  let loginWithToken (config:Configuration) (token, verifier) =
    printfn "%s" token
    printfn "%s" verifier
    let method = "POST"
    let accessTokenUrl = "https://api.twitter.com/oauth/access_token"
    let nonce = Guid.NewGuid().ToString("N")
    let timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds().ToString()
    let auth =
      [ "oauth_consumer_key", config.consumerKey
        "oauth_nonce", nonce
        "oauth_signature_method", "HMAC-SHA1"
        "oauth_timestamp", timestamp
        "oauth_token", token
        "oauth_verifier", verifier
        "oauth_version", "1.0" ]

    let oAuthParamsForSigning =
      auth
      |> List.map (fun (k, v) -> k, urlEncode v)
      |> List.sortBy (fun (k, _) -> k)
      |> List.map merge

    let paramsForSigning =
      [ method
        urlEncode accessTokenUrl
        ampersandJoin oAuthParamsForSigning |> urlEncode ]
      |> ampersandJoin

    let signingKey =
      [ urlEncode config.consumerSecret
        urlEncode token ]
      |> ampersandJoin

    use hmac = new HMACSHA1(toBytes signingKey)
    let signature = toBytes paramsForSigning |> hmac.ComputeHash |> b64Encode

    let oAuthParamsForSending =
      ("oauth_signature", signature) :: auth
      |> List.map (fun (k, v) -> k, urlEncode v |> quoteWrap)
      |> List.sortBy (fun (k, _) -> k)
      |> List.map merge
      |> commaJoin
      |> prependOAuthLead

    let headers =
      [ "Authorization", oAuthParamsForSending ]

    let body = sprintf "oauth_verifier=%s" verifier |> TextRequest
    let r = Http.RequestString(accessTokenUrl, headers=headers, httpMethod=method, body=body, silentHttpErrors=true)
    let qs = parseQs r
    { id = qs.["user_id"]
      name = qs.["screen_name"]
      email = "" }

  let private callbackHandler (config:Configuration) : HttpHandler =
    fun next ctx ->
      (string ctx.Request.Query.["oauth_token"], string ctx.Request.Query.["oauth_verifier"])
      |> loginWithToken config
      |> fun ext -> config.authOk ext next ctx

  let handler config : HttpHandler =
    choose [
      POST >=> route Routes.redirectToTwitterPath >=> config.middleWare >=> iniateRedirectHandler config
      GET >=> route callbackPath >=> callbackHandler config ]
