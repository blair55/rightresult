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

  let getPointsForPrediction result predictionDd =
    let init =
      PredictionPointsMonoid.Init
    match predictionDd with
    | Some (p, true) ->
      if result = p
      then { init with Points=6; DoubleDownCorrectScores=1 }, CorrectScore
      elif getScoreResult result = getScoreResult p
      then { init with Points=2; DoubleDownCorrectResults=1 }, CorrectResult
      else init, Incorrect
    | Some (p, false) ->
      if result = p
      then { init with Points=3; CorrectScores=1 }, CorrectScore
      elif getScoreResult result = getScoreResult p
      then { init with Points=1; CorrectResults=1 }, CorrectResult
      else init, Incorrect
    | None ->
      init, Incorrect

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
