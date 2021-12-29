namespace Shared

open System

module Option =

  let mapF f df =
    function
    | Some x -> f x
    | None -> df

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

module GameweekNo =
  let toGWString (GameweekNo gwno) = sprintf "GW%i" gwno
  let toGWStringLong (GameweekNo gwno) = sprintf "Gameweek %i" gwno
  let previous (GameweekNo gwno) = GameweekNo (gwno - 1)


type YearMonth = YearMonth of year:int * month:int

module Ko =
  type KickOff =
    private | KickOff of DateTime
    member this.Raw =
        let (KickOff ko) = this
        ko

  let create d = DateTimeOffset(d).DateTime |> KickOff

  let hasKickedOff now (KickOff ko) =
    now > ko

  let isLessThanOneHourBeforeKickOff now (KickOff ko) =
    now > ko.AddHours -1.

  let yearMonth (KickOff ko) =
    YearMonth (ko.Year, ko.Month)

type KickOff = Ko.KickOff

type PredictionEditDate = PredictionEditDate of DateTime
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
    member this.Difference =
      let (ScoreLine (Score h, Score a)) = this
      h - a

[<RequireQualifiedAccess>]
type PointVector =
  | Result
  | HomeScore
  | AwayScore
  | GoalDifference
  | BigUp of int
  | DoubleDown

type PointsCategory =
  | CorrectScore
  | CorrectResult
  | Incorrect

type FixtureRecord =
  { Id : FixtureId
    FixtureSetId : FixtureSetId
    GameweekNo : GameweekNo
    KickOff : Ko.KickOff
    TeamLine : TeamLine
    State: FixtureState
    SortOrder : int
  }
and [<RequireQualifiedAccess>] FixtureState =
  | Open of Ko.KickOff
  | InPlay of (ScoreLine * MinutesPlayed)
  | Classified of ScoreLine

and MinutesPlayed = MinutesPlayed of string

and PredictionRecord =
  { PlayerId : PlayerId
    FixtureId : FixtureId
    Modifier : PredictionModifier
    ScoreLine : ScoreLine
    Created : DateTime
  }
and [<RequireQualifiedAccess>] PredictionModifier =
  | None
  | BigUp
  | DoubleDown
and LeagueRecord =
  { LeagueName : LeagueName
    PrivateLeagueId : PrivateLeagueId
  }
and PlayerRecord =
  { Id : PlayerId
    Name : PlayerName
  }

module MinutesPlayed =

  let init = MinutesPlayed "0'"

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
  let [<Literal>] Brentford = "Brentford"
  let [<Literal>] Brighton = "Brighton"
  let [<Literal>] Burnley = "Burnley"
  // let [<Literal>] Cardiff = "Cardiff"
  let [<Literal>] Chelsea = "Chelsea"
  let [<Literal>] CrystalPalace = "Crystal Palace"
  let [<Literal>] Everton = "Everton"
  // let [<Literal>] Fulham = "Fulham"
  // let [<Literal>] Huddersfield = "Huddersfield"
  let [<Literal>] Leeds = "Leeds"
  let [<Literal>] Leicester = "Leicester"
  let [<Literal>] Liverpool = "Liverpool"
  let [<Literal>] ManCity = "Man City"
  let [<Literal>] ManUtd = "Man Utd"
  let [<Literal>] Newcastle = "Newcastle"
  let [<Literal>] Norwich = "Norwich"
  // let [<Literal>] SheffieldUtd = "Sheffield Utd"
  let [<Literal>] Southampton = "Southampton"
  let [<Literal>] Spurs = "Spurs"
  let [<Literal>] Watford = "Watford"
  // let [<Literal>] WestBrom = "West Brom"
  let [<Literal>] WestHam = "West Ham"
  let [<Literal>] Wolves = "Wolves"

  let all =
    [ Arsenal
      AstonVilla
      Brentford
      Brighton
      Burnley
      Chelsea
      CrystalPalace
      Everton
      Leicester
      Leeds
      Liverpool
      ManCity
      ManUtd
      Newcastle
      Norwich
      Southampton
      Spurs
      Watford
      WestHam
      Wolves ]
    |> List.map Team

// view model types

type DocumentId =
    DocumentId of string
and [<CLIMutable>] PlayerNode =
  { Id : string
    Created : DateTime
    LastLogin : DateTime
    Name : string
    Email : string }
and [<CLIMutable>] LeagueNode =
  { Id : string
    Created : DateTime
    Name : string }
and [<CLIMutable>] FixtureSetNode =
  { Id : string
    GameweekNo : int
    Year : int
    Month : int
    Created : DateTime
    IsConcluded : bool }
and [<CLIMutable>] PredictionNode =
  { PlayerId : string
    FixtureId : string
    Created : DateTime
    Modifier: string
    HomeScore : int
    AwayScore : int }
and [<CLIMutable>] FixtureNode =
  { Id : string
    FixtureSetId : string
    Created : DateTime
    GameweekNo : int
    SortOrder : int
    KickOff : DateTime
    HomeTeam : string
    AwayTeam : string
    State: string
    MinutesPlayed: string
    HomeScore : int
    AwayScore : int }

module FixtureState =

  let [<Literal>] OpenStr = "open"
  let [<Literal>] InPlayStr = "inplay"
  let [<Literal>] ClassifiedStr = "classified"

  let toString = function
    | FixtureState.Open _ -> OpenStr
    | FixtureState.InPlay _ -> InPlayStr
    | FixtureState.Classified _ -> ClassifiedStr

  let fromNode (f:FixtureNode) =
    match f.State, f.MinutesPlayed, f.HomeScore, f.AwayScore with
    | OpenStr, _, _, _ -> FixtureState.Open (Ko.create f.KickOff)
    | InPlayStr, mins, home, away -> FixtureState.InPlay (ScoreLine (Score home, Score away), MinutesPlayed mins)
    | ClassifiedStr, _, home, away -> FixtureState.Classified (ScoreLine (Score home, Score away))
    | s, _, _, _ -> failwith <| sprintf "could not parse fixture state %s" s

  let classifiedScoreLine = function
    | FixtureState.Classified result -> Some result
    | _ -> None

  let isClassified = classifiedScoreLine >> Option.isSome

module PredictionModifier =

  module Consts =
    let [<Literal>] None = "none"
    let [<Literal>] BigUp = "bigup"
    let [<Literal>] DoubleDown = "doubledown"

  let toString = function
    | PredictionModifier.None -> Consts.None
    | PredictionModifier.BigUp -> Consts.BigUp
    | PredictionModifier.DoubleDown -> Consts.DoubleDown

  let fromString = function
    | Consts.None -> PredictionModifier.None
    | Consts.BigUp -> PredictionModifier.BigUp
    | Consts.DoubleDown -> PredictionModifier.DoubleDown
    | s -> failwith <| sprintf "could not parse prediction modifier %s" s

  let isDoubleDown = function
    | PredictionModifier.DoubleDown -> true
    | _ -> false

  let isBigUp = function
    | PredictionModifier.BigUp -> true
    | _ -> false

  let isModified = function
    | PredictionModifier.BigUp
    | PredictionModifier.DoubleDown -> true
    | PredictionModifier.None -> false

module FixtureNode =
  let init (FixtureSetId fsId)
           (GameweekNo gwno)
           created
           sortOrder
           { FixtureRecord.Id = FixtureId fId
             KickOff = ko
             TeamLine = TeamLine (Team home, Team away) }  =
    { FixtureNode.Id = string<Guid> fId
      FixtureSetId = string<Guid> fsId
      Created = created
      GameweekNo = gwno
      SortOrder = sortOrder
      KickOff = ko.Raw
      State = FixtureState.OpenStr
      MinutesPlayed = ""
      HomeTeam = home
      AwayTeam = away
      HomeScore = 0
      AwayScore = 0 }

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

type BigUpViewModel =
  { TeamLine: TeamLine
    ScoreLine: ScoreLine
    PlayerId: PlayerId
    PlayerName: PlayerName }

type FixtureDetails =
  { BigUps : BigUpViewModel list
    Home : PremTableRow
    Away : PremTableRow
    FormGuide : (FormFixture option * FormFixture option) list }
  static member Init =
    { BigUps = []; Home = PremTableRow.Init; Away = PremTableRow.Init; FormGuide = [] }
and FormFixture =
  { KickOff : KickOff
    GameweekNo : GameweekNo
    TeamLine : TeamLine
    Opponent : Team
    Scoreline : ScoreLine
    Venue : FormVenue
    Result : FormResult }
and [<RequireQualifiedAccess>] FormResult =
  | W | L | D
and [<RequireQualifiedAccess>] FormVenue =
  | H | A

type KickOffComponents =
  { KickOff : KickOff
    DateAndShortMonth : string
    Group : string
    ShortDay : string
    ClockTime : string }

type FixturePredictionViewModel =
  { Id : FixtureId
    FixtureSetId : FixtureSetId
    GameweekNo : GameweekNo
    SortOrder : int
    KickOff : KickOffComponents
    TeamLine : TeamLine
    State : FixtureState
    Prediction : (ScoreLine * PredictionModifier) option
    PredictionGrid : ScoreLine list list
    BigUpState : BigUpState
    Points : int * PointVector list
    FixtureDetails : FixtureDetails option
    InProgress : bool
    Neighbours: FixtureId option * FixtureId option }
and [<RequireQualifiedAccess>] BigUpState =
  | Unavailable
  | Available
  | Expanded
  | Set
and GlobalGameweekStats =
  { GameweekNo : GameweekNo
    PlayerId : PlayerId
    AveragePoints : int
    MaximumPoints : (PlayerId * int) option
    Player : (int * int * string) option }

and NewFixtureSetViewModel =
  { GameweekNo : GameweekNo
    Fixtures : (KickOffComponents * TeamLine) list }
and PlayerLeagueViewModel =
  { Position : int
    Movement : int
    LeagueName : LeagueName }
and [<CLIMutable>] LeagueTableDoc =
  { LeagueName : LeagueName
    LeagueId : LeagueId
    Members : (PlayerId * LeagueTableMember) list
    MaximumPoints : (PlayerId * int) option
    AvergagePointsWithAtLeastOnePrediction : int
    LeagueTableScope : LeagueTableScope option }
  static member Init leagueId name =
    { LeagueId = leagueId
      LeagueName = name
      Members = []
      MaximumPoints = None
      AvergagePointsWithAtLeastOnePrediction = 0
      LeagueTableScope = None }
and LeagueTableScope =
  | OfMonth of monthDate:DateTime * desc:string
  | IncludesGameweeks of GameweekNo list
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
    Modifier : PredictionModifier
    Points : (int * PointsCategory) option
  }

and PredictionPointsMonoid =
  { Points : int
    CorrectScores : int
    CorrectResults : int }
    // DoubleDownCorrectScores : int
    // DoubleDownCorrectResults : int }
  static member Init =
    { Points=0
      CorrectScores=0
      CorrectResults=0 }
      // DoubleDownCorrectScores=0
      // DoubleDownCorrectResults=0 }
  static member (+) (ppm1:PredictionPointsMonoid, ppm2:PredictionPointsMonoid) =
    { Points=ppm1.Points+ppm2.Points
      CorrectScores=ppm1.CorrectScores+ppm2.CorrectScores
      CorrectResults=ppm1.CorrectResults+ppm2.CorrectResults }
      // DoubleDownCorrectScores=ppm1.DoubleDownCorrectScores+ppm2.DoubleDownCorrectScores
      // DoubleDownCorrectResults=ppm1.DoubleDownCorrectResults+ppm2.DoubleDownCorrectResults }

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

type PlayerGameweekViewModel =
  { PlayerId : PlayerId
    PlayerName : PlayerName
    GameweekNo : GameweekNo
    Neighbours: GameweekNo option * GameweekNo option
    Fixtures : Map<FixtureId, PlayerGameweekViewModelRow>
    GlobalGameweekStats : GlobalGameweekStats option
  }
and PlayerGameweekViewModelRow =
  { FixtureId : FixtureId
    FixtureSetId : FixtureSetId
    TeamLine : TeamLine
    KickOff : KickOffComponents
    SortOrder : int
    Points : int * PointVector list
    State : FixtureState
    Prediction : (ScoreLine * PredictionModifier) option
    IsExpanded : Boolean
  }

type GameweekFixturesViewModel =
  { GameweekNo: GameweekNo
    FixtureSetId: FixtureSetId
    Fixtures: Map<FixtureId, FixturePredictionViewModel>
    IsDoubleDownAvailable: bool
    Neighbours: GameweekNo option * GameweekNo option
    GlobalGameweekStats : GlobalGameweekStats option
  }

[<RequireQualifiedAccess>]
type PredictionAction =
  | SetScoreline of FixtureSetId * FixtureId * ScoreLine
  | IncrementScore of FixtureSetId * FixtureId * PredictTeam
  | DecrementScore of FixtureSetId * FixtureId * PredictTeam
and PredictTeam = Home | Away

type LeagueWindow =
  | Full
  | Week of GameweekNo
  | Month of YearMonth
  | WeekInclusive of GameweekNo

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
  { // getFixtures : int * int -> AppToken -> Ars<Map<FixtureId, FixturePredictionViewModel>>
    getFixturesLength : AppToken -> Ars<int>
    getMaxGameweekNo : AppToken -> Ars<GameweekNo>
    getPlayerLeagues : AppToken -> Ars<Map<PrivateLeagueId, PlayerLeagueViewModel>>
    getPrivateLeagueInfo : PrivateLeagueId -> AppToken -> Ars<PrivateLeagueInfo>
    getLeagueTable : LeagueId -> LeagueWindow -> AppToken -> Ars<LeagueTableDoc>
    getPlayerPointsTotal : PlayerId -> AppToken -> Ars<PredictionPointsMonoid>
    getPlayerFixtureSets : PlayerId -> AppToken -> Ars<PlayerFixtureSetsDoc>
    getMyProfile : AppToken -> Ars<PlayerViewModel>
    getMyPointsTotal : AppToken -> Ars<PredictionPointsMonoid>
    getAllPlayers : AppToken -> Ars<PlayerViewModel list>
    getPlayerInfo : PlayerId -> AppToken -> Ars<PlayerViewModel>
    getPlayerGameweek : PlayerId -> GameweekNo -> AppToken -> Ars<PlayerGameweekViewModel>
    getNewFixtureSet : AppToken -> Ars<NewFixtureSetViewModel>
    getLeagueHistoryFixtureSets : LeagueId -> AppToken -> Ars<LeagueHistoryDoc>
    getLeagueHistoryMonths : LeagueId -> AppToken -> Ars<LeagueHistoryDoc>
    getDateFormat : DateTime -> String -> AppToken -> Ars<String>
    getLeagueMatrix : LeagueId -> GameweekNo -> AppToken -> Ars<MatrixDoc>
    getGlobalGameweekWinner : AppToken -> Ars<GlobalGameweekWinner option>
    getHomePageBigUps : AppToken -> Ars<BigUpViewModel list>
    getRealPremTable : AppToken -> Ars<PremTable>
    getPredictedPremTable : AppToken -> Ars<PremTable>
    getFixtureDetails : AppToken -> FixtureId -> Ars<FixtureDetails>
    getEarliestOpenGwno : AppToken -> Ars<GameweekNo>
    getGameweekFixtures : AppToken -> GameweekNo -> Ars<GameweekFixturesViewModel>
    submitFeedback : string -> AppToken -> Ars<Unit>
    addNewFixtureSet : AppToken -> Ars<Unit>
    prediction : AppToken -> PredictionAction -> Ars<PredictionAction>
    doubleDown : AppToken -> (FixtureSetId * FixtureId) -> Ars<FixtureSetId * FixtureId>
    bigUp : AppToken -> (FixtureSetId * FixtureId) -> Ars<FixtureSetId * FixtureId>
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


module PredictionGrid =

  type Dims = Dims of width:int * height:int

  type private Coords = Coords of x:int * y:int

  module private Coords =

    let incX (Coords (x, y)) = Coords (x + 1, y)
    let decX (Coords (x, y)) = Coords (x - 1, y)
    let incY (Coords (x, y)) = Coords (x, y + 1)
    let decY (Coords (x, y)) = Coords (x, y - 1)
    let unwrap (Coords (x, y)) = x, y

    let max c =
      let x, y = List.map unwrap c |> List.unzip
      Coords (List.max x, List.max y)

    let min c =
      let x, y = List.map unwrap c |> List.unzip
      Coords (List.min x, List.min y)

    let toScoreLine (Coords (x, y)) =
      if x > 0 then ScoreLine(Score (x + y), Score y)
      elif x < 0 then ScoreLine(Score y, Score(Math.Abs(x) + y))
      else ScoreLine(Score y, Score y)

    let fromScoreLine (ScoreLine (Score h, Score a)) =
      Coords (h - a, if h < a then h else a)

  type private BoundingArea = BoundingArea of max:Coords * min:Coords

  module private BoundingArea =

    let fromCoords (Dims (width, height)) (Coords (x, y)) =
      let dmxX, dmnX = width / 2, 0 - (width / 2)
      let dmxY, dmnY = height - 1, 0
      let mxX, mnX =
        if x > dmxX then x, x - (width - 1)
        elif x < dmnX then x + (width - 1), x
        else dmxX, dmnX
      let mxY, mnY =
        if y > dmxY then y, y - dmxY
        else dmxY, dmnY
      BoundingArea (Coords(mxX, mxY), Coords(mnX, mnY))

    let toGrid (BoundingArea (Coords(mxX, mxY), Coords(mnX, mnY))) =
      seq { for y in [ mnY..mxY ] ->
            seq { for x in [ mxX .. -1 .. mnX ] ->
                  Coords(x, y) |> Coords.toScoreLine
            } |> Seq.toList
      } |> Seq.toList

    let fromGrid grid =
      let c = List.collect (List.map (Coords.fromScoreLine)) grid
      BoundingArea (Coords.max c, Coords.min c)

  let private grid dims =
    Coords.fromScoreLine >> BoundingArea.fromCoords dims >> BoundingArea.toGrid

  let init dims =
    Option.map (grid dims) >> Option.defaultWith (fun () -> grid dims ScoreLine.Init)

  let redraw s grid =
    let (BoundingArea (Coords(mxX, mxY), Coords(mnX, mnY))) = BoundingArea.fromGrid grid
    let (Coords (x, y)) = Coords.fromScoreLine s
    let nudge f = List.map (List.map (Coords.fromScoreLine >> f >> Coords.toScoreLine))
    if x > mxX then nudge Coords.incX grid
    elif x < mnX then nudge Coords.decX grid
    elif y > mxY then nudge Coords.incY grid
    elif y < mnY then nudge Coords.decY grid
    else grid


/// TODO:

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
/// - SSRify
/// - femtoify
/// - felizify
/// - deploy fixture ko change
/// - fix push!
/// - homepage gw global leaderboard / player photo
/// - dynamic h1
/// - gw stats by month (pts/avg/max/pos)
/// - only run bg tasks when web available & tasks not running
/// - swipable fixture page
/// - capture score in big up event
/// - remove `correctGwno1Fixtures` func
/// - matrix: full screen, show bigups, points scored shading
/// - add live match data & refresh button?
/// - BADGES
/// - move fixture node module etc from shared into server
/// - ^ then move points into FixtureState.Classified


/// + scorebreakdown on player gw page
/// + gw fixtures header: top score - avg score - my score
/// + scrolling preset boxes
/// + landing page
/// + points
/// + server side tidy
/// + fix serviceworker error
/// + page per fixture
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
