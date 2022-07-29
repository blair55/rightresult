namespace Server.Application

open System
open Shared
open Server.Commands
open Server.Infrastructure

module TestData =

  let buildFixture fsId gwno now i tl =
    let addToKo (now: DateTime) i = now.AddHours(float i+2.)
    // let addToKo (now: DateTime) i = now.AddHours(float 2)
    let ko = Ko.create (addToKo now i)
    { FixtureRecord.Id = FixtureId(Guid.NewGuid())
      FixtureSetId = fsId
      GameweekNo = gwno
      KickOff = ko
      TeamLine = TeamLine tl
      State = FixtureState.Open ko
      SortOrder = 0 }

  let generate (deps: Dependencies) =
    let gwno = FixtureSourcing.getNewGameweekNo deps
    let halveList l = List.splitAt (List.length l / 2) l

    let now = deps.Now()
    let url = Environment.GetEnvironmentVariable "CLIENTHOST"
    let rnd = new Random()

    let fsId = FixtureSetId(Guid.NewGuid())

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

      |> List.mapi (buildFixture fsId gwno now)
      |> List.sortBy (fun f -> f.KickOff)

    let fId1, fId2, fId3, fId4, fId5, fId6, fId7, fId8, fId9, fId10 =
      (fixtures.[0] |> fun f -> f.Id),
      (fixtures.[1] |> fun f -> f.Id),
      (fixtures.[2] |> fun f -> f.Id),
      (fixtures.[3] |> fun f -> f.Id),
      (fixtures.[4] |> fun f -> f.Id),
      (fixtures.[5] |> fun f -> f.Id),
      (fixtures.[6] |> fun f -> f.Id),
      (fixtures.[7] |> fun f -> f.Id),
      (fixtures.[8] |> fun f -> f.Id),
      (fixtures.[9] |> fun f -> f.Id)

    let newPlayerIdStr () =
      Guid
        .NewGuid()
        .ToString("N")
        .Substring(0, rnd.Next(7, 33))

    let buildPlayer () =
      let pIdStr = newPlayerIdStr ()
      PlayerId(sprintf "tst-%s" pIdStr),
      PlayerName(sprintf "tester-%s" pIdStr)

    let buildPlayerLoginCommand (playerId, playerName) =
      PlayerCommand(playerId, Login(playerName, ""))

    let printPlayerLogin (PlayerId playerId, PlayerName playerName) =
      let prts = playerId.Split('-')
      printfn "%s/api/testlogin/%s/%s/%s" url (prts.[0]) (prts.[1]) playerName
      printfn "-----"

    let players =
      [ 1 .. 30 ]
      |> List.map (fun _ -> buildPlayer ())

    players
    |> List.take 3
    |> List.iter printPlayerLogin

    let pId1, pId2, pId3, pId4, pId5, pId6, pId7, pId8, pId9, pId10 =
      fst players.[0],
      fst players.[1],
      fst players.[2],
      fst players.[3],
      fst players.[4],
      fst players.[5],
      fst players.[6],
      fst players.[7],
      fst players.[8],
      fst players.[9]

    let randomScoreLine () =
      ScoreLine(Score (rnd.Next (0, 4)), Score (rnd.Next (0, 4)))

    let playerPredicitons =
      players
      |> List.collect (fun (pId, _) ->
        fixtures
        |> List.mapi (fun i f -> i, f)
        |> List.collect (fun (i, f) ->
            [ PredictionSetCommand(pId, f.FixtureSetId, DatedPredictionCommand(SetScoreLine(randomScoreLine()), f.Id, PredictionEditDate now))
              PredictionSetCommand(pId, f.FixtureSetId, DatedPredictionCommand(SetScoreLine(randomScoreLine()), f.Id, PredictionEditDate now))
              PredictionSetCommand(pId, f.FixtureSetId, DatedPredictionCommand(SetScoreLine(randomScoreLine()), f.Id, PredictionEditDate now)) ]
            @
            (match i with
            | 0 -> [ PredictionSetCommand(pId, f.FixtureSetId, DatedPredictionCommand(BigUp, f.Id, PredictionEditDate now)) ]
            | 1 -> [ PredictionSetCommand(pId, f.FixtureSetId, DatedPredictionCommand(DoubleDown, f.Id, PredictionEditDate now)) ]
            | _ -> [ ])))

    let fixtureResults =
      fixtures
      |> List.collect (fun f ->
        [ FixtureSetCommand(f.FixtureSetId, KickOffFixture f.Id)
          FixtureSetCommand(f.FixtureSetId, ClassifyFixture(f.Id, randomScoreLine())) ])

    List.map buildPlayerLoginCommand players
    @
    [ FixtureSetCommand(fsId, CreateFixtureSet(gwno, fixtures)) ]
    @
    [
      // PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetHomeScore(Score 2), fId1, PredictionEditDate now))
      // PredictionSetCommand(pId, fsId, DatedPredictionCommand(SetAwayScore(Score 1), fId1, PredictionEditDate now))
      // PredictionSetCommand(pId, fsId, DatedPredictionCommand(DoubleDown, fId1, PredictionEditDate now))

      PredictionSetCommand(pId1, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 4, Score 2)), fId1, PredictionEditDate now))
      PredictionSetCommand(pId1, fsId, DatedPredictionCommand(BigUp, fId1, PredictionEditDate now))

      PredictionSetCommand(pId1, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 2, Score 0)), fId2, PredictionEditDate now))

      PredictionSetCommand(pId1, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId3, PredictionEditDate now))
      PredictionSetCommand(pId1, fsId, DatedPredictionCommand(DoubleDown, fId3, PredictionEditDate now))
    ]
    @
    [
      PredictionSetCommand(pId2, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId1, PredictionEditDate now))

      PredictionSetCommand(pId2, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId2, PredictionEditDate now))
      PredictionSetCommand(pId2, fsId, DatedPredictionCommand(BigUp, fId2, PredictionEditDate now))

      PredictionSetCommand(pId2, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId3, PredictionEditDate now))
    ]
    @
    [
      PredictionSetCommand(pId3, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId1, PredictionEditDate now))

      PredictionSetCommand(pId3, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId2, PredictionEditDate now))

      PredictionSetCommand(pId3, fsId, DatedPredictionCommand(SetScoreLine(ScoreLine(Score 3, Score 3)), fId3, PredictionEditDate now))
      PredictionSetCommand(pId3, fsId, DatedPredictionCommand(BigUp, fId3, PredictionEditDate now))
    ]
    // @ (playerPredicitons)
    // @ (fixtureResults)
    @
    [
      FixtureSetCommand(fsId, KickOffFixture fId1)
      FixtureSetCommand(fsId, ClassifyFixture(fId1, ScoreLine(Score 4, Score 2)))

      FixtureSetCommand(fsId, KickOffFixture fId2)
      FixtureSetCommand(fsId, ClassifyFixture(fId2, ScoreLine(Score 2, Score 3)))

      FixtureSetCommand(fsId, KickOffFixture fId3)
      FixtureSetCommand(fsId, ClassifyFixture(fId3, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId4)
      // FixtureSetCommand(fsId, ClassifyFixture(fId4, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId5)
      // FixtureSetCommand(fsId, ClassifyFixture(fId5, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId6)
      // FixtureSetCommand(fsId, ClassifyFixture(fId6, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId7)
      // FixtureSetCommand(fsId, ClassifyFixture(fId7, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId8)
      // FixtureSetCommand(fsId, ClassifyFixture(fId8, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId9)
      // FixtureSetCommand(fsId, ClassifyFixture(fId9, ScoreLine(Score 1, Score 1)))

      // FixtureSetCommand(fsId, KickOffFixture fId10)
      // FixtureSetCommand(fsId, ClassifyFixture(fId10, ScoreLine(Score 1, Score 1)))
    ]