#r "nuget: FSharp.Data"

open System.Text.Json
open FSharp.Data
open FSharp.Data.HttpContentTypes
open FSharp.Data.HttpRequestHeaders

let leagueId = ""

[ "fb-xxx"; "tw-yyy" ]
|> List.map (fun s -> JsonSerializer.Serialize({| PlayerId = s; LeagueId = leagueId |}))
|> List.iter (fun r ->
  Http.Request("https://rightresu.lt/api/addPlayerToLeague", body = TextRequest r, headers = [ ContentType Json ])
  |> ignore)