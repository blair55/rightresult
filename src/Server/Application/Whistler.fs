namespace Server.Application

open System
open Server.Commands
open Server.Queries
open Shared

module Whistler =

  let kickOffFixtures (handle:Command -> Ars<Unit>) (q:Queries) (now:DateTimeOffset) =
    q.getKickedOffFixtures now
    |> List.ofSeq
    |> List.iter (
      fun f ->
        KickOffFixture f.Id
        |> fun cmd -> FixtureSetCommand (f.FixtureSetId, cmd)
        |> handle
        |> Async.RunSynchronously
        |> function
        | Ok _ -> printfn "Fixture kicked off\n%A\n%A" f.Id f.TeamLine
        | Error e -> printfn "ERROR KICKING OFF\n%A" e)