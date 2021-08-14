module Server.Application.BackgroundTasks

open Shared
open Server.Commands
open Server.Queries
open Server.Infrastructure
open Server.Infrastructure.GameweekSources

module Classifier =

  let private classifyFixtures
    desc
    (handle: Command -> Ars<Unit>)
    { Dependencies.FixtureSources = { GameweekResults = GameweekSources.GameweekResults results }
      NonQueries = { updateInPlayFixture = updateInPlayFixture } }
    (fixtures:FixtureRecord list)
    =
    fixtures
    |> List.map (fun f -> f.GameweekNo)
    |> List.distinct
    |> List.collect results
    |> List.iter
         (function
         | GameweekResultState.InPlay (teamLine, scoreLine, mp) ->
             fixtures
             |> List.tryFind (fun f -> f.TeamLine = teamLine)
             |> function
               | Some f -> updateInPlayFixture (f.Id, scoreLine, mp)
               | None -> ()
         | GameweekResultState.Classified (teamLine, scoreLine) ->
             fixtures
             |> List.tryFind (fun f -> f.TeamLine = teamLine)
             |> function
               | Some f ->
                FixtureSetCommand(f.FixtureSetId, ClassifyFixture(f.Id, scoreLine))
                 |> handle
                 |> Async.RunSynchronously
                 |> function
                   | Ok _ -> printfn "%s Fixture Classified\n%A\n%A" desc f scoreLine
                   | Error e -> printfn "ERROR CLASSIFYING\n%A" e
               | None -> ()
         )

  let classifyKickedOffFixtures (handle: Command -> Ars<Unit>) (deps: Dependencies) =
    classifyFixtures "@@@@@@@@@@@@@@@@@ KICKED OFF FIXTURES" handle deps (deps.Queries.getFixturesInPlay() |> List.ofSeq)

  let classifyAllFixtures (handle: Command -> Ars<Unit>) (deps: Dependencies) =
    classifyFixtures "@@@@@@@@@@@@@@@@@ CLASS ALL" handle deps (deps.Queries.getAllFixtures() |> List.ofSeq)

  let classifyFixturesAfterGameweek (handle: Command -> Ars<Unit>) (deps: Dependencies) (GameweekNo gwno) =
    classifyFixtures
      "@@@@@@@@@@@@@@@@@ CLASS AFTER GW"
      handle
      deps
      (deps.Queries.getAllFixtures()
       |> Seq.filter (fun { GameweekNo = GameweekNo g } -> g > gwno)
       |> List.ofSeq)

  let concludeGameweek (handle: Command -> Ars<Unit>) (deps: Dependencies) =
    deps.Queries.getUnconcludedFixtureSets ()
    |> Seq.iter
         (fun (fsId, gwno, fixtures) ->
           if fixtures
              |> Seq.forall (fun f -> FixtureState.isClassified f.State) then
             printfn "CONCLUDING GW %A %A" fsId gwno

             FixtureSetCommand(fsId, ConcludeFixtureSet gwno)
             |> handle
             |> Async.RunSynchronously
             |> ignore<Rresult<Unit>>
           else
             ())


module Whistler =

  let kickOffFixtures (handle: Command -> Ars<Unit>) (deps: Dependencies) =
    deps.Queries.getKickedOffFixtures (deps.Now())
    |> List.ofSeq
    |> List.iter
         (fun f ->
           FixtureSetCommand(f.FixtureSetId, KickOffFixture f.Id)
           |> handle
           |> Async.RunSynchronously
           |> function
             | Ok _ -> printfn "Fixture kicked off\n%A\n%A" f.Id f.TeamLine
             | Error e -> printfn "ERROR KICKING OFF\n%A" e)


let minuteTasks =
  [ Whistler.kickOffFixtures
    Classifier.concludeGameweek
    Classifier.classifyKickedOffFixtures ]

let private editFixtureKickOffs (handle: Command -> Ars<Unit>) deps =
  FixtureSourcing.getEditedFixtureKickOffs deps
  |> List.iter
       (fun (fsId, fId, ko) ->
         FixtureSetCommand(fsId, EditFixtureKickOff(fId, ko))
         |> handle
         |> Async.RunSynchronously
         |> function
           | Ok _ -> printfn "Editing fixture kick off\n%A" fId
           | Error e -> printfn "ERROR EDITING FIXTURE KICK OFF\n%A" e)

let dailyTasks = [ editFixtureKickOffs ]
