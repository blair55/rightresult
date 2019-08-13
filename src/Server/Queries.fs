namespace Server

open System
open Shared

module Queries =

  type Queries =
    { getKickedOffFixtures : DateTimeOffset -> FixtureRecord seq
      getAllFixtures : unit -> FixtureRecord seq
      getFixturesInLatestFixtureSet : unit -> (FixtureSetId * GameweekNo * (FixtureRecord seq)) option
      getFixturesAwaitingResults : unit -> FixtureRecord seq
      getPlayerPredictionsByFixture : PlayerId -> (FixtureNode * PredictionNode option) seq
      getPlayerPredictionForFixture : PlayerId -> FixtureId -> PredictionNode option
      getFixtureSetAndEarliestKo : FixtureSetId -> FixtureNode * DateTimeOffset
      getPredictionsForPlayer : PlayerId -> (FixtureNode * PredictionNode) list
      getPredictionsForPlayerInFixtureSet : FixtureSetId -> PlayerId -> (FixtureNode * PredictionNode) list
      getPredictionsForPlayerInMonth : (int * int) -> PlayerId -> (FixtureNode * PredictionNode) list
      getPlayersInPrivateLeague : PrivateLeagueId -> PlayerNode list
      getAllPlayers : unit -> PlayerNode list
      getFixtureSet : FixtureSetId -> FixtureSetNode
      getPrivateLeagues : unit -> LeagueNode seq
      getPrivateLeaguesAndMembers : unit -> (LeagueNode * (PlayerNode seq)) seq

      getFixturesLength : unit -> int
      getLeaguesPlayerIsIn : PlayerId -> LeagueRecord seq
      getPlayerFixtureSet : PlayerId -> FixtureSetId -> (FixtureRecord * PredictionRecord option) seq
      getPlayer : PlayerId -> PlayerRecord option
      getPrivateLeague : PrivateLeagueId -> LeagueRecord option
      getFixtureByTeams : Team -> Team -> FixtureRecord
      getMaxGameweekNo : unit -> GameweekNo option
    }

  type NonQueries =
    { kickOffFixture : FixtureId -> unit
      editFixtureKo : FixtureId * KickOff -> unit
      createFixtureSet : FixtureSetNode -> unit
      createFixture : FixtureSetId -> FixtureNode -> unit
      classifyFixture : FixtureId * ScoreLine -> unit
      createPrediction : PredictionRecord -> unit
      deletePredictionSet : PlayerId * FixtureSetId -> unit
    }