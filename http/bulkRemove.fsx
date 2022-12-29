#r "nuget: FSharp.Data"

open System.Text.Json
open FSharp.Data
open FSharp.Data.HttpContentTypes
open FSharp.Data.HttpRequestHeaders

[ "" ]
|> List.map (fun s -> JsonSerializer.Serialize({| PlayerId = s |}))
|> List.iter (fun r ->
  Http.Request("https://rightresu.lt/api/removePlayer", body = TextRequest r, headers = [ ContentType Json ])
  |> ignore)