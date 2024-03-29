namespace Server

open Shared

module Points =

  type ScoreResult =
    | HomeWin
    | AwayWin
    | Draw

  let getScoreResult (ScoreLine (home, away)) =
    if home > away then HomeWin
    elif home < away then AwayWin
    else Draw

  let isReverse = function
    | HomeWin, AwayWin
    | AwayWin, HomeWin -> true
    | _ -> false

  let pointVectorFunction = function
    | PointVector.Result -> (+) 2
    | PointVector.HomeScore
    | PointVector.AwayScore
    | PointVector.GoalDifference -> (+) 1
    | PointVector.BigUp p -> (+) p
    | PointVector.DoubleDown -> (*) 2

  let getPointVectors ((ScoreLine (hr, ar)) as result) ((ScoreLine (hp, ap)) as pred) modifier =
    seq {
      let fixtureResult = getScoreResult result
      let predictionResult = getScoreResult pred
      if isReverse (fixtureResult, predictionResult) then yield! []
      else
        if fixtureResult = predictionResult then yield PointVector.Result
        if hr = hp then yield PointVector.HomeScore
        if ar = ap then yield PointVector.AwayScore
        if result.Difference = pred.Difference then yield PointVector.GoalDifference
        match modifier with
        | PredictionModifier.BigUp when result = pred -> yield PointVector.BigUp 5
        | PredictionModifier.BigUp when fixtureResult = predictionResult -> yield PointVector.BigUp 3
        | PredictionModifier.DoubleDown -> yield PointVector.DoubleDown
        | _ -> yield! []
    } |> List.ofSeq

  let sumVectorPoints =
    List.fold (fun p v -> pointVectorFunction v p) 0

  let getPointsForPrediction result pred vectors =
    let init =
      { PredictionPointsMonoid.Init with Points = sumVectorPoints vectors }
    if result = pred
    then { init with CorrectScores=1 }
    elif getScoreResult result = getScoreResult pred
    then { init with CorrectResults=1 }
    else init

  let getHomeAndAwayPremTableRowDiff (ScoreLine (Score homeScore, Score awayScore)) =
    { PremTableRow.Init with Played = 1 }
    |> fun defaultRow ->
    ScoreLine (Score homeScore, Score awayScore)
    |> getScoreResult
    |> function
    | HomeWin ->
      { defaultRow with Won  = 1; GoalsFor = homeScore; GoalsAgainst = awayScore; Points = 3 },
      { defaultRow with Lost = 1; GoalsFor = awayScore; GoalsAgainst = homeScore }
    | AwayWin ->
      { defaultRow with Lost = 1; GoalsFor = homeScore; GoalsAgainst = awayScore },
      { defaultRow with Won  = 1; GoalsFor = awayScore; GoalsAgainst = homeScore; Points = 3 }
    | Draw ->
      { defaultRow with Drawn = 1; GoalsFor = homeScore; GoalsAgainst = awayScore; Points = 1 },
      { defaultRow with Drawn = 1; GoalsFor = awayScore; GoalsAgainst = homeScore; Points = 1 }

  let buildTable { PremTable.Rows = rows } results =
    (rows, results)
    ||> List.fold (fun table (TeamLine (homeTeam, awayTeam), scoreLine) ->
      let (homeRowDiff, awayRowDiff) =
        getHomeAndAwayPremTableRowDiff scoreLine
      table
      |> Map.add homeTeam (table.[homeTeam] + homeRowDiff)
      |> Map.add awayTeam (table.[awayTeam] + awayRowDiff))
    |> Map.toList
    |> List.sortByDescending (fun (_, row) -> row.Points, row.GoalsFor - row.GoalsAgainst, row.GoalsFor)
    |> List.mapi (fun i (team, row) -> team, { row with Position = i+1 })
    |> fun r -> { PremTable.Rows = r |> Map.ofList }

module KickoffComponents =

  let build (ko:KickOff) =
    { KickOff = ko
      Group = ko.Raw.ToString("ddd d MMM yyyy")
      DateAndShortMonth = ko.Raw.ToString("dd MMM")
      ShortDay = ko.Raw.ToString("ddd")
      ClockTime = ko.Raw.ToString("HH:mm") }
