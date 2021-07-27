namespace Server.Application

open System
open FSharp.Data
open Server.Commands
open Server.Queries
open Shared
open Server.Infrastructure
open Server.Infrastructure.Time

module FixtureSourcing =

  type PremFixtures =
    JsonProvider<Sample="Application/PremFixturesSample.json">

  let private premFixturesUrl =
    sprintf "https://fantasy.premierleague.com/api/fixtures/?event=%i"

  open Shared.Teams

  let private premTeamIdToName = function
    | 1 -> Arsenal
    | 2 -> AstonVilla
    | 3 -> Brentford
    | 4 -> Brighton
    | 5 -> Burnley
    | 6 -> Chelsea
    | 7 -> CrystalPalace
    | 8 -> Everton
    | 9 -> Leeds
    | 10 -> Leicester
    | 11 -> Liverpool
    | 12 -> ManCity
    | 13 -> ManUtd
    | 14 -> Newcastle
    | 15 -> Norwich
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
    // |> Seq.map(fun f -> toUkTime f.KickoffTime.DateTime |> KickOff, toTeam f.TeamH, toTeam f.TeamA)
    |> Seq.map(fun f -> f.KickoffTime |> KickOff, toTeam f.TeamH, toTeam f.TeamA)
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
      |> List.map (fun (ko, h, a) -> ko, KickOff.groupFormat ko, h, a)
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
          KickOff = ko
          TeamLine = TeamLine (h, a)
          ScoreLine = None
          SortOrder = i
          HasKickedOff = false })
      |> fun fixtures -> GameweekNo gwno, fixtures
      |> CreateFixtureSet
      |> fun fscmd -> FixtureSetCommand (fsId, fscmd)))

  let getEditedFixtureKickOffs (deps:Dependencies) =
    deps.Queries.getUnconcludedFixtureSets ()
    |> List.ofSeq
    |> List.collect (fun (_, GameweekNo gwno, legacyFixtures) ->
      let latestFixtures = getNewPremGwFixtures gwno
      legacyFixtures
      |> List.choose (fun leg ->
          latestFixtures
          |> List.tryFind (fun (_, h, a) -> TeamLine (h, a) = leg.TeamLine)
          |> Option.map (fun (ko, _, _) -> leg, ko))
      |> List.filter (fun (leg, ko) -> leg.KickOff <> ko)
      |> List.map (fun (leg, ko) -> leg.FixtureSetId, leg.Id, ko))
