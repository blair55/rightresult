namespace Server.Application

open System
open Shared
open Server.Commands
open Server.Infrastructure

module TestData =

  let buildFixture fsId gwno tl ko =
    { FixtureRecord.Id = FixtureId(Guid.NewGuid())
      FixtureSetId = fsId
      GameweekNo = gwno
      KickOff = KickOff ko
      TeamLine = TeamLine tl
      ScoreLine = None
      SortOrder = 0
      HasKickedOff = false }

  let generate (deps: Dependencies) =
    let gwno = FixtureSourcing.getNewGameweekNo deps
    let fsId = FixtureSetId(Guid.NewGuid())
    let now = deps.Now()
    let halveList l = List.splitAt (List.length l / 2) l

    let fixtures =
      Teams.all
      |> halveList
      ||> List.zip
      |> halveList
      |> fun (closeds, opens) ->
           (closeds
            |> List.mapi (fun i tl -> buildFixture fsId gwno tl (now.AddHours(float -i))))
           @ (opens
              |> List.mapi (fun i tl -> buildFixture fsId gwno tl (now.AddHours(float i))))
      |> List.sortByDescending (fun f -> f.KickOff)

    let fId1 = fixtures.[0] |> fun f -> f.Id
    let fId2 = fixtures.[1] |> fun f -> f.Id
    let fId3 = fixtures.[2] |> fun f -> f.Id

    let pIdStr =
      Guid.NewGuid().ToString().Substring(0, 7)

    let url =
      Environment.GetEnvironmentVariable "CLIENTHOST"

    let playerName, playerPrefix = "tester", "test"
    printfn $"{url}/api/testlogin/{playerPrefix}/{pIdStr}/tester"

    let pId =
      PlayerId(sprintf $"{playerPrefix}-{pIdStr}")

    [ FixtureSetCommand(fsId, CreateFixtureSet(gwno, fixtures))
      PlayerCommand(pId, Login(PlayerName playerName, ""))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 2), fId1, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 1), fId1, PredictionEditDate now))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 3), fId2, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 3), fId2, PredictionEditDate now))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 0), fId3, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 1), fId3, PredictionEditDate now))

      FixtureSetCommand(fsId, KickOffFixture fId1)
      FixtureSetCommand(fsId, ClassifyFixture(fId1, ScoreLine(Score 2, Score 1)))

      FixtureSetCommand(fsId, KickOffFixture fId2)
      FixtureSetCommand(fsId, ClassifyFixture(fId2, ScoreLine(Score 1, Score 1)))

      FixtureSetCommand(fsId, KickOffFixture fId3) ]
