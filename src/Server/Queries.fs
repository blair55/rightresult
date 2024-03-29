namespace Server

open System
open Shared

module Queries =

  type Queries =
    { getKickedOffFixtures : DateTime -> FixtureRecord seq
      getAllFixtures : unit -> FixtureRecord seq
      getFixturesInFixtureSet : FixtureSetId -> FixtureRecord list
      getAllFixtureSetsAndFixtures : unit -> (FixtureSetId * GameweekNo * (FixtureRecord list)) list
      getFixturesInPlay : unit -> FixtureRecord seq
      getOpenFixtures : unit -> FixtureRecord seq
      getPlayerPredictionsByFixture : PlayerId -> (FixtureRecord * PredictionRecord option) seq
      getPlayerPredictionForFixture : PlayerId -> FixtureId -> PredictionRecord option
      getPredictionsForPlayer : PlayerId -> (FixtureRecord * PredictionRecord) list
      getPredictionsForPlayerInFixtureSet : FixtureSetId -> PlayerId -> (FixtureRecord * PredictionRecord) list
      getPredictionsForPlayerInMonth : YearMonth -> PlayerId -> (FixtureRecord * PredictionRecord) list
      getPlayersInPrivateLeague : PrivateLeagueId -> PlayerRecord list
      getAllPlayers : unit -> PlayerRecord list
      getFixtureSetGameweekNo : FixtureSetId -> GameweekNo
      getGameweekNoFixtureSet : GameweekNo -> FixtureSetId option
      getGameweekNos : unit -> GameweekNo list
      getFixtureRecord : FixtureId -> FixtureRecord
      getUnconcludedFixtureSets : unit -> (FixtureSetId * GameweekNo * FixtureRecord list) seq
      getPrivateLeagues : unit -> LeagueRecord seq
      getPrivateLeaguesAndMembers : unit -> (LeagueRecord * (PlayerRecord seq)) seq
      getFixturesLength : unit -> int
      getLeaguesPlayerIsIn : PlayerId -> LeagueRecord list
      getPlayerFixtureSet : PlayerId -> GameweekNo -> (FixtureRecord * PredictionRecord option) seq
      getPlayer : PlayerId -> PlayerRecord option
      getPrivateLeague : PrivateLeagueId -> LeagueRecord option
      getFixtureByTeams : Team -> Team -> FixtureRecord
      getMaxGameweekNo : unit -> GameweekNo option
      getEarliestOpenGwno : unit -> GameweekNo option
      getPredictionsAggregate : FixtureId -> PredictionsAggregate
      getFixturesForTeam : Team -> FixtureRecord seq
      getHomePageBigUps : unit -> (FixtureRecord * PredictionRecord * PlayerRecord) list
    }

  type NonQueries =
    { kickOffFixture : FixtureId -> unit
      editFixtureKo : FixtureId * KickOff -> unit
      editFixtureSortOrder : FixtureId * int -> unit
      createFixtureSet : FixtureSetNode -> unit
      createFixture : FixtureSetId -> FixtureNode -> unit
      updateInPlayFixture : FixtureId * ScoreLine * MinutesPlayed -> unit
      classifyFixture : FixtureId * ScoreLine -> unit
      createPrediction : PredictionRecord -> unit
      deletePredictionSet : PlayerId * FixtureSetId -> unit
      deleteFixture : FixtureId -> unit
      concludeFixtureSet : FixtureSetId -> unit
    }