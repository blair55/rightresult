namespace Server.Application

open System
open Shared
open Server.Commands
open Server.Infrastructure
open Server.Infrastructure.Time

module TestData =

  let buildFixture fsId gwno tl ko =
    { FixtureRecord.Id = FixtureId(Guid.NewGuid())
      FixtureSetId = fsId
      GameweekNo = gwno
      KickOff = Ko.create ko
      TeamLine = TeamLine tl
      State = FixtureState.Open (Ko.create ko)
      SortOrder = 0 }

  let generate (deps: Dependencies) =
    let gwno = FixtureSourcing.getNewGameweekNo deps
    let fsId = FixtureSetId(Guid.NewGuid())
    let now = deps.Now()
    let halveList l = List.splitAt (List.length l / 2) l

    let addToKo (now: DateTime) i =
      now.AddHours(float i)

    let fixtures =
      Teams.all
      |> halveList
      ||> List.zip
      // |> halveList
      // |> fun (closeds, opens) ->
      //      (closeds
      //       |> List.mapi (fun i tl -> buildFixture fsId gwno tl (addToKo now -i)))
      //      @ (opens
      //         |> List.mapi (fun i tl -> buildFixture fsId gwno tl (addToKo now i)))

      |> List.mapi (fun i tl -> buildFixture fsId gwno tl (addToKo now i))
      |> List.sortBy (fun f -> f.KickOff)

    let fId1 = fixtures.[0] |> fun f -> f.Id
    let fId2 = fixtures.[1] |> fun f -> f.Id
    let fId3 = fixtures.[2] |> fun f -> f.Id

    let newPlayerIdStr() =
      Guid.NewGuid().ToString().Substring(0, 7)

    let buildPlayerId prefix pIdStr =
      PlayerId(sprintf $"{prefix}-{pIdStr}")

    let buildPlayerLoginCommand () =
      let pId = newPlayerIdStr()
      let name = PlayerName $"tester-{pId}"
      PlayerCommand(buildPlayerId "tst" pId, Login(name, ""))

    let pIdStr = newPlayerIdStr()
    let url = Environment.GetEnvironmentVariable "CLIENTHOST"
    let playerName, playerPrefix = $"tester-{pIdStr}", "tst"
    let pId = buildPlayerId playerPrefix pIdStr
    printfn $"{url}/api/testlogin/{playerPrefix}/{pIdStr}/{playerName}"

    ([1..30] |> List.map (fun _ -> buildPlayerLoginCommand()))
    @
    [ FixtureSetCommand(fsId, CreateFixtureSet(gwno, fixtures))
      PlayerCommand(pId, Login(PlayerName playerName, ""))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 2), fId1, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 1), fId1, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(DoubleDown, fId1, PredictionEditDate now))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 3), fId2, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 3), fId2, PredictionEditDate now))
      PredictionSetCommand(pId, fsId, DatedPredictionCommand(BigUp, fId2, PredictionEditDate now))

      PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 0, Score 2)), fId3, PredictionEditDate now))

      FixtureSetCommand(fsId, KickOffFixture fId1)
      FixtureSetCommand(fsId, ClassifyFixture(fId1, ScoreLine(Score 2, Score 1)))

      FixtureSetCommand(fsId, KickOffFixture fId2)
      FixtureSetCommand(fsId, ClassifyFixture(fId2, ScoreLine(Score 1, Score 1)))

      FixtureSetCommand(fsId, KickOffFixture fId3)
      FixtureSetCommand(fsId, ClassifyFixture(fId3, ScoreLine(Score 1, Score 1)))
    ]
