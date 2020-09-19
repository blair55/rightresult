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

type PushSubscription =
  { Endpoint : string
    Keys : PushSubscriptionKeys }
and PushSubscriptionKeys =
  { P256dh : string
    Auth : string }

module Teams =

  let [<Literal>] Arsenal = "Arsenal"
  let [<Literal>] AstonVilla = "Aston Villa"
  // let [<Literal>] Bournemouth = "Bournemouth"
  let [<Literal>] Brighton = "Brighton"
  let [<Literal>] Burnley = "Burnley"
  // let [<Literal>] Cardiff = "Cardiff"
  let [<Literal>] Chelsea = "Chelsea"
  let [<Literal>] CrystalPalace = "Crystal Palace"
  let [<Literal>] Everton = "Everton"
  let [<Literal>] Fulham = "Fulham"
  // let [<Literal>] Huddersfield = "Huddersfield"
  let [<Literal>] Leicester = "Leicester"
  let [<Literal>] Leeds = "Leeds"
  let [<Literal>] Liverpool = "Liverpool"
  let [<Literal>] ManCity = "Man City"
  let [<Literal>] ManUtd = "Man Utd"
  let [<Literal>] Newcastle = "Newcastle"
  // let [<Literal>] Norwich = "Norwich"
  let [<Literal>] SheffieldUtd = "Sheffield Utd"
  let [<Literal>] Southampton = "Southampton"
  let [<Literal>] Spurs = "Spurs"
  // let [<Literal>] Watford = "Watford"
  let [<Literal>] WestBrom = "West Brom"
  let [<Literal>] WestHam = "West Ham"
  let [<Literal>] Wolves = "Wolves"

  let all =
    [ Arsenal
      AstonVilla
      Brighton
      Burnley
      Chelsea
      CrystalPalace
      Everton
      Fulham
      Leicester
      Leeds
      Liverpool
      ManCity
      ManUtd
      Newcastle
      SheffieldUtd
      Southampton
      Spurs
      WestBrom
      WestHam
      Wolves ]
    |> List.map Team

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
    Created : DateTimeOffset
    IsConcluded : bool }
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
  | WeekInclusive of int

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

type GlobalGameweekWinner =
  { PlayerId : PlayerId
    GameweekNo : GameweekNo
    Member : LeagueTableMember
  }

type PremTable =
  { Rows : Map<Team, PremTableRow> }
  static member Init =
    { Rows =
        Teams.all
        |> List.map (fun t -> t, PremTableRow.Init)
        |> Map.ofList }
and PremTableRow =
  { Position : int
    Played : int
    Won : int
    Lost : int
    Drawn : int
    GoalsFor : int
    GoalsAgainst : int
    Points : int }
  static member Init =
    { Position = 0; Played = 0; Won = 0; Lost = 0; Drawn = 0; GoalsFor = 0; GoalsAgainst = 0; Points = 0 }
  static member (+) (a:PremTableRow, b:PremTableRow) =
    { Position = 0
      Played = a.Played + b.Played
      Won = a.Won + b.Won
      Lost = a.Lost + b.Lost
      Drawn = a.Drawn + b.Drawn
      GoalsFor = a.GoalsFor + b.GoalsFor
      GoalsAgainst = a.GoalsAgainst + b.GoalsAgainst
      Points = a.Points + b.Points }


type FixtureDetails =
  { KickOff : KickOff
    Id : FixtureId
    Home : FixtureDetailsColumn
    Away : FixtureDetailsColumn }
  static member Init fId ko =
    { Id = fId; KickOff = ko; Home = FixtureDetailsColumn.Init; Away = FixtureDetailsColumn.Init }
and FixtureDetailsColumn =
  { Team : Team
    PremTableRow : PremTableRow
    FormGuide : FormFixture list }
  static member Init =
    { Team = Team ""
      PremTableRow = PremTableRow.Init
      FormGuide = [] }
and FormFixture =
  { KickOff : KickOff
    Venue : FormVenue
    Result : FormResult
    GoalsFor : Score
    GoalsAgainst : Score }
and FormVenue =
  | H | A
and FormResult =
  | W | L | D

/// separate because sourced by db query,
/// its not document at rest. Also maybe we don't
/// want if fixture has not yet kicked off?
/// If so, this endpoint should be protected
/// such that this info is not returned until ko
and PredictionsAggregate =
  { AvgHomeScore : decimal
    AvgAwayScore : decimal
    HomeWinCount : int
    AwayWinCount : int
    DrawCount : int }
///       +
/// +     +
/// + + + + +
/// W W D W L
/// -   - - -
///       - -
/// h a h h a
///
/// toggle: HomeAway/All
/// opacity gradient with recency


/// MATCH (p:Prediction)-[:FOR_FIXTURE]->(f:Fixture)
/// WHERE f.Id = 'c785a15e-7e59-4da1-a201-afe61916701c'
/// RETURN
/// avg(p.HomeScore) as AvgHomeScore,
/// avg(p.AwayScore) as AvgAwayScore,
/// count(case p.HomeScore > p.AwayScore when true then 1 end) as HomeWins,
/// count(case p.HomeScore = p.AwayScore when true then 1 end) as Draws,
/// count(case p.HomeScore < p.AwayScore when true then 1 end) as AwayWins


type Document =
  | LeagueTableDocument of LeagueId * LeagueWindow
  | LeagueAllFixtureSetHistory of LeagueId
  | LeagueAllMonthHistory of LeagueId
  | PlayerFixtureSetsDocument of PlayerId
  | PlayerPushSubscriptions
  | Matrix of LeagueId * GameweekNo
  | GlobalGameweekWinner
  | RealPremTable
  | PredictedPremTable of PlayerId
  | FormGuideDocument of Team
  | FixtureDetailsDocument of FixtureId

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
    getGlobalGameweekWinner : AppToken -> Ars<GlobalGameweekWinner option>
    getRealPremTable : AppToken -> Ars<PremTable>
    getPredictedPremTable : AppToken -> Ars<PremTable>
    getFixtureDetails : AppToken -> FixtureId -> Ars<FixtureDetails>
    submitFeedback : string -> AppToken -> Ars<Unit>
    addNewFixtureSet : AppToken -> Ars<Unit>
    prediction : AppToken -> PredictionAction -> Ars<PredictionAction>
    doubleDown : AppToken -> (FixtureSetId * FixtureId) -> Ars<FixtureSetId * FixtureId>
    removeDoubleDown : AppToken -> FixtureSetId -> Ars<FixtureSetId>
    createLeague : AppToken -> LeagueName -> Ars<PrivateLeagueId>
    joinLeague : AppToken -> PrivateLeagueId -> Ars<PrivateLeagueId>
    leaveLeague : AppToken -> PrivateLeagueId -> Ars<Unit>
    subscribeToPush : AppToken -> PushSubscription -> Ars<Unit>
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

/// TODO:

/// - add team to form guide
/// - league history multiple winners
/// - league history paging
/// - protect add fixtures
/// - fixture prediction stats
/// - handle unhappy notifcations reg path
/// - homeplayer week history & average points
/// - player rank, points, average pts chart
/// - player highlights section - DDs and outliers
/// - new awards added to hp ?
/// - xss ?
/// - undo allowing multiple league join events per player / revert 6250be3

/// + add fixture score to matrix
/// + real & predicted prem tables
/// + league position delta
/// + fixture form guide
/// + homepage winning player
/// + pwa
/// + swap elasticsearch for in-memory
/// + league position grouping
/// + league matrix
/// + gameweek in omnifixture view
/// + fix invite social icons
/// + matrix column sort order
/// + homepage player global rank
/// + https
/// + auto add results
/// + month/week league history
/// + fixture paging
/// + password protect eventstore
/// + productionize & dns
/// + feedback/survey
/// + other player's predictions/points
/// + Favicon
/// + DateTime.Now???
/// + Link home in nav bar
/// + prevent negative score
/// + Cancel button on Join League & Create League views
/// + auto add fixtures
/// + sort fixtures by ko
/// + redirect url on login
/// + home page
/// + league page
/// + club badges
/// + selected tab
/// + error msgs & handle all Result|Error cases
/// + serve index.html on all unmatched paths
/// + use proper facebook login using private key
/// + twitter login
/// + double down
/// + share/join league
///
/// - edit result (ApplyResult cmd to Classified fixture)
///   -> how does this work with adding points to players in leagues?
/// - only league admin can remove league, capabilities?
/// - elasticsearch async endpoints?
///
/// + edit fixture cmd/event (e.g. ko)
/// + event versioning
