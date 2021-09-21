namespace Server.Application

open System
open Server.Commands
open Server.Queries
open Shared
open Server.Infrastructure
open GameweekSources

module KickoffComponents =

  let build (ko:KickOff) =
    { KickOff = ko
      Group = ko.Raw.ToString("ddd d MMM yyyy")
      ShortDay = ko.Raw.ToString("ddd")
      ClockTime = ko.Raw.ToString("HH:mm") }

module FixtureSourcing =

  let getNewGameweekNo (deps: Dependencies) =
    deps.Queries.getMaxGameweekNo ()
    |> function
      | Some (GameweekNo gw) -> gw
      | _ -> 0
    |> (+) 1
    |> GameweekNo

  let getNewFixtureSetViewModel
    ({ Dependencies.FixtureSources = { NewGameweek = NewGameweek ngw } } as deps)
    =
    let gwno = getNewGameweekNo deps

    ngw gwno
    |> List.map (fun (tl, ko) -> KickoffComponents.build ko, tl)
    |> fun items ->
         { NewFixtureSetViewModel.GameweekNo = gwno
           Fixtures = items }

  let addNewFixtureSet ({ Dependencies.FixtureSources = { NewGameweek = NewGameweek ngw } } as deps) =
    let gwno = getNewGameweekNo deps
    let fsId = FixtureSetId(Guid.NewGuid())

    ngw gwno
    |> List.sortBy (fun (_, ko) -> ko)
    |> List.mapi
         (fun i (tl, ko) ->
           { FixtureRecord.Id = FixtureId(Guid.NewGuid())
             FixtureSetId = fsId
             GameweekNo = gwno
             KickOff = ko
             TeamLine = tl
             State = FixtureState.Open ko
             SortOrder = i })
    |> fun fixtures -> FixtureSetCommand(fsId, CreateFixtureSet(gwno, fixtures))

  let getEditedFixtureKickOffs
    ({ Dependencies.FixtureSources = { NewGameweek = NewGameweek ngw }
       Queries = q })
    =
    q.getUnconcludedFixtureSets ()
    |> List.ofSeq
    |> List.collect
         (fun (_, gwno, legacyFixtures) ->
           let latestFixtures = ngw gwno

           legacyFixtures
           |> List.choose
                (fun leg ->
                  latestFixtures
                  |> List.tryFind (fun (tl, _) -> tl = leg.TeamLine)
                  |> Option.map (fun (_, ko) -> leg, ko))
           |> List.filter (fun (leg, ko) -> leg.KickOff <> ko)
           |> List.map (fun (leg, ko) -> leg.FixtureSetId, leg.Id, ko))
