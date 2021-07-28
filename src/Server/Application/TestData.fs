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
    let now = deps.Now

    let fixtures =
      Shared.Teams.all
      |> List.pairwise
      |> fun l -> List.splitAt (List.length l / 2) l
      |> fun (closeds, opens) ->
           (closeds
            |> List.mapi (fun i tl -> buildFixture fsId gwno tl (now().AddHours(float -i))))
           @ (opens
              |> List.mapi (fun i tl -> buildFixture fsId gwno tl (now().AddHours(float i))))

    [ FixtureSetCommand(fsId, CreateFixtureSet(gwno, fixtures))
      FixtureSetCommand(fsId, KickOffFixture(List.head fixtures |> fun f -> f.Id)) ]
