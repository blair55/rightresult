namespace Shared

open System

type RemoteError =
  | LoginProblem of string
  | InvalidToken
  | ValidationError of string
  | ServerSideError of string
  | StateTransitionError of string
  | CommandApplicationError of string
  | WrongEventVersionError of string
  | AsyncError of exn

type ExternalAuth =
  { id : string
    name : string
    email : string }

type Rresult<'a> = Result<'a, RemoteError>
type Ars<'a> = Async<Rresult<'a>>

// domain types
type PlayerId = PlayerId of string
type PlayerName = PlayerName of string
type LeagueId =
  | GlobalLeague
  | PrivateLeague of PrivateLeagueId
and PrivateLeagueId = PrivateLeagueId of Guid
type LeagueName = LeagueName of string
type GameweekNo = GameweekNo of int
type FixtureSetId = FixtureSetId of Guid
type FixtureId = FixtureId of Guid
type Team = Team of string
type TeamLine = TeamLine of home:Team * away:Team
type KickOff = KickOff of DateTimeOffset
type PredictionEditDate = PredictionEditDate of DateTimeOffset
type Score =
  | Score of int
    member this.Inc () =
      let (Score s) = this
      Score (s+1)
    member this.Dec () =
      let (Score s) = this
      Score (s-1)
type ScoreLine =
  | ScoreLine of home:Score * away:Score
    static member Init =
      ScoreLine (Score 0, Score 0)

type PointsCategory =
  | CorrectScore
  | CorrectResult
  | Incorrect

type FixtureRecord =
  { Id : FixtureId
    FixtureSetId : FixtureSetId
    GameweekNo : GameweekNo
    KickOff : KickOff
    TeamLine : TeamLine
    ScoreLine : ScoreLine option
    SortOrder : int
  }
and PredictionRecord =
  { PlayerId : PlayerId
    FixtureId : FixtureId
    IsDoubleDown : bool
    ScoreLine : ScoreLine
    Created : DateTimeOffset
  }
and LeagueRecord =
  { LeagueName : LeagueName
    PrivateLeagueId : PrivateLeagueId
  }
and PlayerRecord =
  { Id : PlayerId
    Name : PlayerName
  }

type AppToken =
  AppToken of string

type ClientSafePlayer =
  { Name : string
    Id : PlayerId
    Token : AppToken }

// view model types

type DocumentId =
    DocumentId of string
and [<CLIMutable>] PlayerNode =
  { Id : string
    Created : DateTimeOffset
    LastLogin : DateTimeOffset
    Name : string
    Email : string }
and [<CLIMutable>] LeagueNode =
  { Id : string
    Created : DateTimeOffset
    Name : string }
and [<CLIMutable>] FixtureSetNode =
  { Id : string
    GameweekNo : int
    Year : int
    Month : int
    Created : DateTimeOffset }
and [<CLIMutable>] PredictionNode =
  { PlayerId : string
    FixtureId : string
    Created : DateTimeOffset
    IsDoubleDown : bool
    HomeScore : int
    AwayScore : int }
and [<CLIMutable>] FixtureNode =
  { Id : string
    FixtureSetId : string
    Created : DateTimeOffset
    GameweekNo : int
    SortOrder : int
    KickOff : DateTimeOffset
    HomeTeam : string
    AwayTeam : string
    HasKickedOff : bool
    HasResult : bool
    HomeScore : int
    AwayScore : int }
and FixturePredictionViewModel =
  { Id : FixtureId
    FixtureSetId : FixtureSetId
    GameweekNo : GameweekNo
    SortOrder : int
    KickOff : KickOff
    FormattedKickOff : string
    TeamLine : TeamLine
    IsDoubleDown : bool
    State : FixtureState
    Prediction : ScoreLine option
    InProgress : bool
    IsDoubleDownAvailable : bool }
and FixtureState =
  | Open
  | KickedOff
  | Classified of result:ScoreLine * points:int * PointsCategory
and NewFixtureSetViewModel =
  { GameweekNo : GameweekNo
    Fixtures : (KickOff * string * Team * Team) list }
and PlayerLeagueViewModel =
  { Position : int
    Movement : int
    LeagueName : LeagueName }
and [<CLIMutable>] LeagueTableDoc =
  { LeagueName : LeagueName
    Members : (PlayerId * LeagueTableMember) list }
  static member Init name =
    { LeagueName = name
      Members = [] }
and LeagueTableMember =
  { Position : int
    Movement : int
    PlayerName : PlayerName
    Points : PredictionPointsMonoid }
  static member Init name =
    { Position = 0
      Movement = 0
      PlayerName = name
      Points = PredictionPointsMonoid.Init }

and MatrixDoc =
  { FixtureSetId : FixtureSetId
    LeagueId : LeagueId
    LeagueName : LeagueName
    GameweekNo : GameweekNo
    Columns : Map<FixtureId, MatrixFixture>
    Rows : Map<PlayerId, MatrixPlayer>
  }
and MatrixFixture =
  { TeamLine : TeamLine
    KickOff : KickOff
    SortOrder : int
    State : MatrixFixtureState
  }
and MatrixFixtureState =
  | Open
  | KickedOff
  | Classified of ScoreLine
and MatrixPlayer =
  { PlayerName : PlayerName
    Predictions : Map<FixtureId, MatrixPrediction>
    TotalPoints : int
  }
and MatrixPrediction =
  { Prediction : ScoreLine
    IsDoubleDown : bool
    Points : (int * PointsCategory) option
  }

and PredictionPointsMonoid =
  { Points : int
    CorrectScores : int
    CorrectResults : int
    DoubleDownCorrectScores : int
    DoubleDownCorrectResults : int }
  static member Init =
    { Points=0
      CorrectScores=0
      CorrectResults=0
      DoubleDownCorrectScores=0
      DoubleDownCorrectResults=0 }
  static member (+) (ppm1:PredictionPointsMonoid, ppm2:PredictionPointsMonoid) =
    { Points=ppm1.Points+ppm2.Points
      CorrectScores=ppm1.CorrectScores+ppm2.CorrectScores
      CorrectResults=ppm1.CorrectResults+ppm2.CorrectResults
      DoubleDownCorrectScores=ppm1.DoubleDownCorrectScores+ppm2.DoubleDownCorrectScores
      DoubleDownCorrectResults=ppm1.DoubleDownCorrectResults+ppm2.DoubleDownCorrectResults }
and [<CLIMutable>] PlayerViewModel =
  { Id : PlayerId
    Name : PlayerName
  }

type PlayerFixtureSetsDoc =
  { PlayerId : PlayerId
    FixtureSets : Map<FixtureSetId, PlayerFixtureSetsDocRow>
  }
  static member Init playerId =
    { PlayerId = playerId
      FixtureSets = Map.empty
    }
and PlayerFixtureSetsDocRow =
  { GameweekNo : GameweekNo
    AveragePoints : float
    PlayerPoints : PredictionPointsMonoid
  }
  static member Init gwno =
    { GameweekNo = gwno
      AveragePoints = 0.
      PlayerPoints = PredictionPointsMonoid.Init
    }

type PlayerFixtureSetViewModel =
  { PlayerId : PlayerId
    FixtureSetId : FixtureSetId
    PlayerName : PlayerName
    GameweekNo : GameweekNo
    AveragePoints : int
    TotalPoints : PredictionPointsMonoid
    Rows : PlayerFixtureSetKickedOffViewModelRow list
  }
and PlayerFixtureSetKickedOffViewModelRow =
  { FixtureId : FixtureId
    TeamLine : TeamLine
    KickOff : KickOff
    KickOffString : string
    SortOrder : int
    Points : PredictionPointsMonoid
    ResultAndPoints : (ScoreLine * PointsCategory) option
    Prediction : (ScoreLine * bool) option
  }

type PredictionAction =
  PredictionAction of FixtureSetId * FixtureId * PredictTeam * PredictVector
and PredictTeam = Home | Away
and PredictVector = Inc | Dec

type LeagueWindow =
  | Full
  | Week of int
  | Month of int * int

type LeagueHistoryDoc =
  Map<LeagueWindow, LeagueHistoryUnitWinner>
and LeagueHistoryUnitWinner =
  { Description : string
    PlayerName : PlayerName
    Points : PredictionPointsMonoid
  }

type PrivateLeagueInfo =
  { LeagueName : LeagueName
  }

type Document =
  | LeagueTableDocument of LeagueId * LeagueWindow
  | LeagueAllFixtureSetHistory of LeagueId
  | LeagueAllMonthHistory of LeagueId
  | PlayerFixtureSetsDocument of PlayerId
  | Matrix of LeagueId * GameweekNo

type IProtocol =
  { getFixtures : int * int -> AppToken -> Ars<Map<FixtureId, FixturePredictionViewModel>>
    getFixturesLength : AppToken -> Ars<int>
    getMaxGameweekNo : AppToken -> Ars<GameweekNo>
    getPlayerLeagues : AppToken -> Ars<Map<PrivateLeagueId, PlayerLeagueViewModel>>
    getPrivateLeagueInfo : PrivateLeagueId -> AppToken -> Ars<PrivateLeagueInfo>
    getLeagueTable : LeagueId -> LeagueWindow -> AppToken -> Ars<LeagueTableDoc>
    getPlayerPointsTotal : PlayerId -> AppToken -> Ars<PredictionPointsMonoid>
    getPlayerFixtureSets : PlayerId -> AppToken -> Ars<PlayerFixtureSetsDoc>
    getAllPlayers : AppToken -> Ars<PlayerViewModel list>
    getPlayerInfo : PlayerId -> AppToken -> Ars<PlayerViewModel>
    getPlayerFixtureSet : PlayerId -> FixtureSetId -> AppToken -> Ars<PlayerFixtureSetViewModel>
    getNewFixtureSet : AppToken -> Ars<NewFixtureSetViewModel>
    getLeagueHistoryFixtureSets : LeagueId -> AppToken -> Ars<LeagueHistoryDoc>
    getLeagueHistoryMonths : LeagueId -> AppToken -> Ars<LeagueHistoryDoc>
    getDateFormat : DateTime -> String -> AppToken -> Ars<String>
    getLeagueMatrix : LeagueId -> GameweekNo -> AppToken -> Ars<MatrixDoc>
    submitFeedback : string -> AppToken -> Ars<Unit>
    addNewFixtureSet : AppToken -> Ars<Unit>
    prediction : AppToken -> PredictionAction -> Ars<PredictionAction>
    doubleDown : AppToken -> (FixtureSetId * FixtureId) -> Ars<FixtureSetId * FixtureId>
    removeDoubleDown : AppToken -> FixtureSetId -> Ars<FixtureSetId>
    createLeague : AppToken -> LeagueName -> Ars<PrivateLeagueId>
    joinLeague : AppToken -> PrivateLeagueId -> Ars<PrivateLeagueId>
    leaveLeague : AppToken -> PrivateLeagueId -> Ars<Unit>
  }

module Routes =

  let builder typeName methodName =
    sprintf "/api/%s/%s" typeName methodName

  let redirectToFacebookPath = "/api/facebook/redirect"
  let redirectToTwitterPath = "/api/twitter/redirect"
  let redirectPathKey = "redirectPath"

module Global =

  let leagueName = LeagueName "Global League"
  let identifier = "global"

module KickOff =

  let groupFormat (KickOff ko) =
    ko.DateTime.ToString("ddd MMM d, yyyy")

  let isLessThan (KickOff ko) now =
    ko < now

module Teams =

  let [<Literal>] Arsenal = "Arsenal"
  let [<Literal>] Bournemouth = "Bournemouth"
  let [<Literal>] Brighton = "Brighton"
  let [<Literal>] Burnley = "Burnley"
  let [<Literal>] Cardiff = "Cardiff"
  let [<Literal>] Chelsea = "Chelsea"
  let [<Literal>] CrystalPalace = "Crystal Palace"
  let [<Literal>] Everton = "Everton"
  let [<Literal>] Fulham = "Fulham"
  let [<Literal>] Huddersfield = "Huddersfield"
  let [<Literal>] Leicester = "Leicester"
  let [<Literal>] Liverpool = "Liverpool"
  let [<Literal>] ManCity = "Man City"
  let [<Literal>] ManUtd = "Man Utd"
  let [<Literal>] Newcastle = "Newcastle"
  let [<Literal>] Southampton = "Southampton"
  let [<Literal>] Spurs = "Spurs"
  let [<Literal>] Watford = "Watford"
  let [<Literal>] WestHam = "West Ham"
  let [<Literal>] Wolves = "Wolves"

(***TODO:

  - leave league event should effect docs?
  - league history multiple winners
  - league history paging
  - fixture prediction stats
  - protect add fixtures
  - league position delta
  - fixture form guide
  - chart: player rank/points/average?
  - homepage winning player
  - homepage best performing league
  - homeplayer week history & average points
  - pwa

  + swap elasticsearch for in-memory
  + league position grouping
  + league matrix
  + gameweek in omnifixture view
  + fix invite social icons
  + matrix column sort order
  + homepage player global rank
  + https
  + auto add results
  + month/week league history
  + fixture paging
  + password protect eventstore
  + productionize & dns
  + feedback/survey
  + other player's predictions/points
  + Favicon
  + DateTime.Now???
  + Link home in nav bar
  + prevent negative score
  + Cancel button on Join League & Create League views
  + auto add fixtures
  + sort fixtures by ko
  + redirect url on login
  + home page
  + league page
  + club badges
  + selected tab
  + error msgs & handle all Result|Error cases
  + serve index.html on all unmatched paths
  + use proper facebook login using private key
  + twitter login
  + double down
  + share/join league

  - edit result (ApplyResult cmd to Classified fixture)
    -> how does this work with adding points to players in leagues?
  - only league admin can remove league, capabilities?
  - elasticsearch async endpoints?

  + edit fixture cmd/event (e.g. ko)
  + event versioning
*)
