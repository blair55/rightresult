namespace Server

open Shared
open System

module Events =

  type Event =
    // player
    | PlayerCreated of PlayerId * PlayerName * email:string
    | PlayerLoggedIn of PlayerId
    | PlayerRemoved of PlayerId
    // league
    | LeagueCreated of PrivateLeagueId * LeagueName * PlayerId
    | LeagueRenamed of PrivateLeagueId * LeagueName
    | LeagueJoined of PrivateLeagueId * PlayerId
    | LeagueLeft of PrivateLeagueId * PlayerId
    | LeagueRemoved of PrivateLeagueId
    //fixture set
    | FixtureSetCreated of FixtureSetId * GameweekNo * FixtureRecord list
    | FixtureKoEdited of FixtureSetId * FixtureId * KickOff
    | FixtureKickedOff of FixtureSetId * FixtureId
    | FixtureClassified of FixtureSetId * FixtureId * ScoreLine
    // prediction set
    | PredictionSetOverwritten of source:PlayerId * destination:PlayerId * FixtureSetId
    | PredictionSetDoubleDownRemoved of PlayerId * FixtureSetId
    | PredictionCreated of PlayerId * FixtureSetId * FixtureId * ScoreLine
    | PredictionHomeScoreSet of PlayerId * FixtureSetId * FixtureId * Score
    | PredictionAwayScoreSet of PlayerId * FixtureSetId * FixtureId * Score
    | PredictionHomeIncd of PlayerId * FixtureSetId * FixtureId
    | PredictionAwayIncd of PlayerId * FixtureSetId * FixtureId
    | PredictionHomeDecd of PlayerId * FixtureSetId * FixtureId
    | PredictionAwayDecd of PlayerId * FixtureSetId * FixtureId
    | PredictionDoubleDownApplied of PlayerId * FixtureSetId * FixtureId

  type StreamId =
    StreamId of String

  type EventVersion =
    EventVersion of int64

  let initEventVersion =
    EventVersion -1L

  let incVer (EventVersion version) =
    EventVersion (version + 1L)

  type DatedEvent =
    DatedEvent of Event * DateTimeOffset

  type ReadEvents =
    StreamId -> Ars<EventVersion * DatedEvent list>

  type StoreEvents =
    StreamId -> EventVersion -> Event list -> Rresult<Unit>

module HandlerHelper =

  let eventErr event entity =
    sprintf "FUCKNIG EVENT ERROR %A on %A" event entity
    |> fun e -> printfn "%s" e; e
    |> StateTransitionError |> Error

  let cmdErr cmd entity =
    sprintf "FUCKNIG CMD ERROR %A on %A" cmd entity
    |> fun e -> printfn "%s" e; e
    |> CommandApplicationError |> Error
