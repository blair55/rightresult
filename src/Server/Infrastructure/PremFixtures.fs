namespace Server.Infrastructure

open FSharp.Data
open Shared
open System
open Server.Infrastructure.Time

module PremFixtures =

  open Shared.Teams

  type PremFixtures = JsonProvider<Sample="Infrastructure/PremFixturesSample.json", RootName="Fixture">

  let private premFixturesUrl =
    sprintf "https://fantasy.premierleague.com/api/fixtures/?event=%i"

  let private premTeamIdToName =
    function
    | 1 -> Arsenal
    | 2 -> AstonVilla
    | 3 -> Brentford
    | 4 -> Brighton
    | 5 -> Burnley
    | 6 -> Chelsea
    | 7 -> CrystalPalace
    | 8 -> Everton
    | 9 -> Leicester
    | 10 -> Leeds
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

  let private toTeam = premTeamIdToName >> Team

  let newPremFixtures (GameweekNo no) =
    premFixturesUrl no
    |> PremFixtures.Load
    |> List.ofSeq

  let (|FixtureKickoff|) (f: PremFixtures.Fixture) =
    Ko.create (toUkTime f.KickoffTime.DateTime)

  let (|FixtureTeamLine|) (f: PremFixtures.Fixture) =
    TeamLine(toTeam f.TeamH, toTeam f.TeamA)

  let (|FixtureScoreLine|_|) (f: PremFixtures.Fixture) =
    (f.TeamHScore, f.TeamAScore)
    ||> Option.map2 (fun h a -> ScoreLine(Score h, Score a))

  let (|FixtureInPlay|_|) (f: PremFixtures.Fixture) =
    match f.Started, f.FinishedProvisional with
    | true, false -> Some()
    | _ -> None

  let (|FixtureClassified|_|) (f: PremFixtures.Fixture) =
    match f.Started, f.FinishedProvisional with
    | true, true -> Some()
    | _ -> None


module PremPulse =

  type PremPulse = JsonProvider<Sample="Infrastructure/PremPulseSample.json", SampleIsList=true, RootName="Pulse">

  let private getPremPulseJson pulseId =
    sprintf "%s/football/fixtures/%i" (Environment.GetEnvironmentVariable "PULSEAPI") pulseId
    |> fun url -> Http.RequestString(url, headers = [ "origin", "https://www.premierleague.com" ])
    |> PremPulse.Parse

  let (|PulseMinutes|_|) (p: PremPulse.Pulse) =
    match p.Phase.String, p.Clock with
    | Some "H", _ -> Some(MinutesPlayed "HT")
    | _, Some c -> Some(MinutesPlayed(c.Label.TrimEnd('0')))
    | _ -> None

  let (|PulseScoreLine|_|) (p: PremPulse.Pulse) =
    let score i =
      Array.tryItem i p.Teams
      |> Option.bind (fun t -> Option.map Score t.Score)

    (score 0, score 1)
    ||> Option.map2 (fun h a -> ScoreLine(h, a))

  let (|PulseInPlay|_|) (f: PremFixtures.PremFixtures.Fixture) =
    let pulse = getPremPulseJson (f.PulseId)

    match pulse, pulse with
    | PulseMinutes mp, PulseScoreLine sl -> Some(mp, sl)
    | _ -> None


module GameweekSources =

  open PremFixtures
  open PremPulse

  type NewGameweek = NewGameweek of (GameweekNo -> (TeamLine * KickOff) list)

  type GameweekResults = GameweekResults of (GameweekNo -> GameweekResultState list)

  and [<RequireQualifiedAccess>] GameweekResultState =
    | InPlay of (TeamLine * ScoreLine * MinutesPlayed)
    | Classified of (TeamLine * ScoreLine)

  let private newFixture f =
    match f, f with
    | FixtureTeamLine tl, FixtureKickoff ko -> tl, ko

  let private resultState f =
    match f, f, f with
    | FixtureClassified, FixtureTeamLine tl, FixtureScoreLine sl -> Some(GameweekResultState.Classified(tl, sl))
    | FixtureInPlay, FixtureTeamLine tl, PulseInPlay (mp, sl) -> Some(GameweekResultState.InPlay(tl, sl, mp))
    | _ -> None

  type FixtureSources =
    { NewGameweek: NewGameweek
      GameweekResults: GameweekResults }

  let fixturesSources =
    { NewGameweek = NewGameweek(newPremFixtures >> List.map newFixture)
      GameweekResults = GameweekResults(newPremFixtures >> List.choose resultState) }
