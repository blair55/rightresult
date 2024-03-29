﻿namespace Server

open Shared

module Commands =

  type Command =
    | PlayerCommand of PlayerId * PlayerCommand
    | PrivateLeagueCommand of PrivateLeagueId * PrivateLeagueCommand
    | FixtureSetCommand of FixtureSetId * FixtureSetCommand
    | PredictionSetCommand of PlayerId * FixtureSetId * PredictionSetCommand
  and PlayerCommand =
    | Login of PlayerName * email:string
    | SubscribeToPush of PushSubscription
    | Remove
  and PrivateLeagueCommand =
    | CreateLeague of PlayerId * LeagueName
    | RenameLeague of LeagueName
    | JoinLeague of PlayerId
    | LeaveLeague of PlayerId
    | RemoveLeague of PlayerId
  and FixtureSetCommand =
    | CreateFixtureSet of GameweekNo * FixtureRecord list
    | ConcludeFixtureSet of GameweekNo
    | EditFixtureKickOff of FixtureId * KickOff
    | KickOffFixture of FixtureId
    | ClassifyFixture of FixtureId * ScoreLine
    | ReclassifyFixture of FixtureId
    | AppendFixture of FixtureRecord
    | RemoveOpenFixture of FixtureId
  and PredictionSetCommand =
    | RemoveDoubleDown of PredictionEditDate
    | DatedPredictionCommand of PredictionCommand * FixtureId * PredictionEditDate
    | OverwritePredictionSet of sourcePlayer:PlayerId
    | PutPredictions of (FixtureId * ScoreLine) list
  and PredictionCommand =
    | DoubleDown
    | BigUp
    | SetScoreLine of ScoreLine
    | SetHomeScore of Score
    | SetAwayScore of Score
    | SimplePredictionCommand of SimplePredictionCommand
  and SimplePredictionCommand =
    | IncHomeScore
    | IncAwayScore
    | DecHomeScore
    | DecAwayScore
