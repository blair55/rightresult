namespace Server.HttpHandlers

open System
open FSharp.Data
open Server.Commands
open Server.Queries
open Shared
open Server.Infrastructure.Persistence

module FixtureSourcing =

  type PremFixtures =
    JsonProvider<Sample="HttpHandlers/PremFixturesSample.json">

  let private premFixturesUrl =
    sprintf "https://fantasy.premierleague.com/drf/fixtures/?event=%i"

  open Shared.Teams

  let private premTeamIdToName = function
    | 1 -> Arsenal
    | 2 -> Bournemouth
    | 3 -> Brighton
    | 4 -> Burnley
    | 5 -> Cardiff
    | 6 -> Chelsea
    | 7 -> CrystalPalace
    | 8 -> Everton
    | 9 -> Fulham
    | 10 -> Huddersfield
    | 11 -> Leicester
    | 12 -> Liverpool
    | 13 -> ManCity
    | 14 -> ManUtd
    | 15 -> Newcastle
    | 16 -> Southampton
    | 17 -> Spurs
    | 18 -> Watford
    | 19 -> WestHam
    | 20 -> Wolves
    | _ -> failwith "Unrecognised team id"

  let private toTeam =
    premTeamIdToName >> Team

  let private getNewPremGwFixtures no =
    PremFixtures.Load(premFixturesUrl no)
    |> Seq.map(fun f -> new DateTimeOffset(f.KickoffTime), f.TeamH |> premTeamIdToName, f.TeamA |> premTeamIdToName)
    |> Seq.toList

  let getNewPremGwResults no =
    PremFixtures.Load(premFixturesUrl no)
    |> Seq.filter (fun f -> f.Started && f.FinishedProvisional)
    |> Seq.map (fun f ->
        TeamLine (toTeam f.TeamH, toTeam f.TeamA),
        ScoreLine (Score (f.TeamHScore.JsonValue.AsInteger()), Score (f.TeamAScore.JsonValue.AsInteger())))
    |> Seq.toList

  let private getNewGameweekNo (deps:Dependencies) =
    deps.Queries.getMaxGameweekNo ()
    |> function
    | Some (GameweekNo gw) -> gw
    | _ -> 0
    |> (+) 1

  let getNewFixtureSetViewModel (deps:Dependencies) =
    deps
    |> (getNewGameweekNo
      >> (fun gwno ->
      getNewPremGwFixtures gwno
      |> List.map (fun (ko, h, a) -> KickOff ko, KickOff.groupFormat (KickOff ko), Team h, Team a)
      |> fun items -> { NewFixtureSetViewModel.GameweekNo = GameweekNo gwno; Fixtures = items }))

  let addNewFixtureSet (deps:Dependencies) =
    deps
    |> (getNewGameweekNo
      >> (fun gwno ->
      FixtureSetId (Guid.NewGuid())
      |> fun fsId ->
      getNewPremGwFixtures gwno
      |> List.sortBy (fun (ko, _, _) -> ko)
      |> List.mapi (fun i (ko, h, a) ->
        { FixtureRecord.Id = FixtureId (Guid.NewGuid())
          FixtureSetId = fsId
          GameweekNo = GameweekNo gwno
          KickOff = KickOff ko
          TeamLine = TeamLine (Team h, Team a)
          ScoreLine = None
          SortOrder = i })
      |> fun fixtures -> GameweekNo gwno, fixtures
      |> CreateFixtureSet
      |> fun fscmd -> FixtureSetCommand (fsId, fscmd)))

module Classifier =

  let private classifyFixtures (handle:Command -> Ars<Unit>) (q:Queries) (fixturesFunc:Queries -> FixtureRecord seq) =
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
        | Ok _ -> printfn "Fixture Classified\n%A\n%A" f scoreLine
        | Error e -> printfn "ERROR CLASSIFYING\n%A" e
      | None -> ())

  let classifyKickedOffFixtures (handle:Command -> Ars<Unit>) (q:Queries) =
    classifyFixtures handle q (fun q -> q.getFixturesAwaitingResults ())

  let classifyAllFixtures (handle:Command -> Ars<Unit>) (q:Queries) =
    classifyFixtures handle q (fun q -> q.getAllFixtures ())

  let classifyFixturesAfterGameweek (handle:Command -> Ars<Unit>) (q:Queries) (GameweekNo gwno) =
    classifyFixtures handle q (fun q -> q.getAllFixtures () |> Seq.filter(fun { GameweekNo = GameweekNo g } -> g > gwno))

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

