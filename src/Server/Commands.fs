namespace Server

open Shared

module Commands =

  type Command =
    | PlayerCommand of PlayerId * PlayerCommand
    | PrivateLeagueCommand of PrivateLeagueId * PrivateLeagueCommand
    | FixtureSetCommand of FixtureSetId * FixtureSetCommand
    | PredictionSetCommand of PlayerId * FixtureSetId * PredictionSetCommand
  and PlayerCommand =
    | Login of PlayerName * email:string
    | Remove
  and PrivateLeagueCommand =
    | CreateLeague of PlayerId * LeagueName
    | JoinLeague of PlayerId
    | LeaveLeague of PlayerId
    | RemoveLeague of PlayerId
  and FixtureSetCommand =
    | CreateFixtureSet of GameweekNo * FixtureRecord list
    | EditFixtureKickOff of FixtureId * KickOff
    | KickOffFixture of FixtureId
    | ClassifyFixture of FixtureId * ScoreLine
  and PredictionSetCommand =
    | RemoveDoubleDown of PredictionEditDate
    | DatedPredictionCommand of PredictionCommand * FixtureId * PredictionEditDate
    | OverwritePredictionSet of sourcePlayer:PlayerId
  and PredictionCommand =
    | DoubleDown
    | SetHomeScore of Score
    | SetAwayScore of Score
    | SimplePredictionCommand of SimplePredictionCommand
  and SimplePredictionCommand =
    | IncHomeScore
    | IncAwayScore
    | DecHomeScore
    | DecAwayScore
