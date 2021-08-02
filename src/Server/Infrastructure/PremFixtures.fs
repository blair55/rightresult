namespace Server.Infrastructure

open FSharp.Data
open Shared
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

  let private toTeam = premTeamIdToName >> Team

  let private newPremFixtures (GameweekNo no) =
    premFixturesUrl no
    |> PremFixtures.Load
    |> List.ofSeq

  let (|FixtureKickoff|) (f: PremFixtures.Fixture) =
    KickOff(toUkTime f.KickoffTime.DateTime)

  let (|FixtureTeamLine|) (f: PremFixtures.Fixture) =
    TeamLine(toTeam f.TeamH, toTeam f.TeamA)

  let (|FixtureScoreLine|_|) (f: PremFixtures.Fixture) =
    (f.TeamHScore, f.TeamAScore)
    ||> Option.map2 (fun h a -> ScoreLine(Score h, Score a))

  let (|FixtureHasResult|_|) (f: PremFixtures.Fixture) =
    match f.Started, f, f with
    | true, FixtureScoreLine sl, FixtureTeamLine tl -> Some(tl, sl)
    | _ -> None

  let (|FixtureIsFinished|_|) (f: PremFixtures.Fixture) =
    match f.Started, f.FinishedProvisional, f, f with
    | true, true, FixtureScoreLine sl, FixtureTeamLine tl -> Some(tl, sl)
    | _ -> None

  type NewGameweek = NewGameweek of (GameweekNo -> (TeamLine * KickOff) list)

  type GameweekResults = GameweekResults of (GameweekNo -> GameweekResultState list)

  and [<RequireQualifiedAccess>] GameweekResultState =
    | InPlay of (TeamLine * ScoreLine * MinutesPlayed)
    | Classified of (TeamLine * ScoreLine)

  let private newFixture f =
    match f, f with
    | FixtureTeamLine tl, FixtureKickoff ko -> tl, ko

  let private resultState (f: PremFixtures.Fixture) =
    match f.Started, f.FinishedProvisional, f, f with
    | true, false, FixtureScoreLine sl, FixtureTeamLine tl ->
      Some(GameweekResultState.InPlay(tl, sl, MinutesPlayed f.Minutes))
    | true, true, FixtureScoreLine sl, FixtureTeamLine tl -> Some(GameweekResultState.Classified(tl, sl))
    | _ -> None


  type FixtureSources =
    { NewGameweek: NewGameweek
      GameweekResults: GameweekResults }


  let fixturesSources =
    { NewGameweek = NewGameweek(newPremFixtures >> List.map newFixture)
      GameweekResults = GameweekResults(newPremFixtures >> List.choose resultState) }
