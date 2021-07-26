module Server.Application.BackgroundTasks

open System
open System.Threading
open Shared
open Server.Commands
open Server.Queries
open Microsoft.Extensions.Hosting

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


let backgroundTasks handleCommand queries now =
  Whistler.kickOffFixtures handleCommand queries (now())
  Classifier.concludeGameweek handleCommand queries
  Classifier.classifyKickedOffFixtures handleCommand queries

type RecurringTasks (inf, now) =

  interface IHostedService with
    member __.StartAsync ct =
      printfn "STARTING RECURRING TASKS"
      let (_, _, _, _, queries, _, _, handleCommand) = inf
      async {
        while not ct.IsCancellationRequested do
          backgroundTasks handleCommand queries now
          return! Async.Sleep 60000
      } |> Async.StartAsTask |> ignore
      Tasks.Task.CompletedTask
    member __.StopAsync _ =
      printfn "STOPPING RECURRING TASKS"
      Tasks.Task.CompletedTask
