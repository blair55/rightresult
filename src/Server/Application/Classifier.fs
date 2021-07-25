namespace Server.Application

open Server.Commands
open Server.Queries
open Shared

module Classifier =

  let private classifyFixtures desc (handle:Command -> Ars<Unit>) (q:Queries) (fixturesFunc:Queries -> FixtureRecord seq) =
    let fixtures =
      fixturesFunc q
      |> List.ofSeq
    fixtures
    |> List.map (fun f -> f.GameweekNo)
    |> List.distinct
    |> List.map (fun (GameweekNo gwno) -> FixtureSourcing.getNewPremGwResults gwno)
    |> List.collect id
    |> List.iter (fun (teamLine, scoreLine) ->
      fixtures
      |> List.tryFind (fun f -> f.TeamLine = teamLine)
      |> function
      | Some f ->
        ClassifyFixture (f.Id, scoreLine)
        |> fun cmd -> FixtureSetCommand (f.FixtureSetId, cmd)
        |> handle
        |> Async.RunSynchronously
        |> function
        | Ok _ -> printfn "%s Fixture Classified\n%A\n%A" desc f scoreLine
        | Error e -> printfn "ERROR CLASSIFYING\n%A" e
      | None -> ())

  let classifyKickedOffFixtures (handle:Command -> Ars<Unit>) (q:Queries) =
    classifyFixtures "@@@@@@@@@@@@@@@@@ KICKED OFF FIXTURES" handle q (fun q -> q.getFixturesAwaitingResults ())

  let classifyAllFixtures (handle:Command -> Ars<Unit>) (q:Queries) =
    classifyFixtures "@@@@@@@@@@@@@@@@@ CLASS ALL" handle q (fun q -> q.getAllFixtures ())

  let classifyFixturesAfterGameweek (handle:Command -> Ars<Unit>) (q:Queries) (GameweekNo gwno) =
    classifyFixtures "@@@@@@@@@@@@@@@@@ CLASS AFTER GW" handle q (fun q -> q.getAllFixtures () |> Seq.filter(fun { GameweekNo = GameweekNo g } -> g > gwno))

  let concludeGameweek (handle:Command -> Ars<Unit>) (q:Queries) =
    q.getUnconcludedFixtureSets ()
    |> Seq.iter (fun (fsId, gwno, fixtures) ->
      if fixtures |> Seq.forall (fun f -> f.ScoreLine.IsSome) then
        printfn "CONCLUDING GW %A %A" fsId gwno
        FixtureSetCommand (fsId, ConcludeFixtureSet gwno)
        |> handle
        |> Async.RunSynchronously
        |> ignore
      else ())