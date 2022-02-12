namespace Areas.Components

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared

module PredictionScore =

  let private predScoreEmpty =
    div [ Class "pred-score-empty" ] [
      str ""
    ]

  let private predScore (s: string) = div [ Class "pred-score" ] [ str s ]

  let private predScoreline (ScoreLine (Score h, Score a)) =
    [ predScore (string<int> h)
      predScore (string<int> a) ]

  let private predDoubleDown =
    div [ Class "pred-dd" ] [
      Fa.i [ Fa.Solid.AngleDoubleDown ] []
    ]

  let private predBigUp =
    div [ Class "pred-bigup" ] [
      Fa.i [ Fa.Solid.AngleDoubleUp ] []
    ]

  let element pred points =
    let shading =
      match (float points / float 10) * 100. with
      | pc when pc = 0 -> "0"
      | pc when pc <= 10 -> "10"
      | pc when pc <= 20 -> "20"
      | pc when pc <= 30 -> "30"
      | pc when pc <= 40 -> "40"
      | pc when pc <= 50 -> "50"
      | pc when pc <= 60 -> "60"
      | pc when pc <= 70 -> "70"
      | pc when pc <= 80 -> "80"
      | pc when pc <= 90 -> "90"
      | _ -> "100"
      |> sprintf "point-shade-%s"
    match pred with
    | Some (sl, PredictionModifier.BigUp) -> predScoreline sl @ [ predBigUp ]
    | Some (sl, PredictionModifier.DoubleDown) -> predScoreline sl @ [ predDoubleDown ]
    | Some (sl, PredictionModifier.None) -> predScoreline sl
    | None -> [ predScoreEmpty; predScoreEmpty ]
    |> div [ Class ("pred-score-container " + shading) ]

  module ResultScore =

    let element (state:FixtureState) =
      div
        [ Class "pred-score-container" ]
        (match state with
         | FixtureState.Open _ -> [ predScore "_"; predScore "_" ]
         | FixtureState.InPlay (sl, _)
         | FixtureState.Classified sl -> predScoreline sl)
