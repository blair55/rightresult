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
      Fa.i [ Fa.Solid.AngleDoubleDown
             Fa.Size Fa.FaExtraSmall ] []
    ]

  let private predBigUp =
    div [ Class "pred-bigup" ] [
      Fa.i [ Fa.Solid.AngleDoubleUp
             Fa.Size Fa.FaExtraSmall ] []
    ]

  let element pred =
    match pred with
    | Some (sl, PredictionModifier.BigUp) -> predScoreline sl @ [ predBigUp ]
    | Some (sl, PredictionModifier.DoubleDown) -> predScoreline sl @ [ predDoubleDown ]
    | Some (sl, PredictionModifier.None) -> predScoreline sl
    | None -> [ predScoreEmpty; predScoreEmpty ]
    |> div [ Class "pred-score-container" ]

  module ResultScore =

    let element (state:FixtureState) =
      div
        [ Class "pred-score-container" ]
        (match state with
         | FixtureState.Open _ -> [ predScore "_"; predScore "_" ]
         | FixtureState.InPlay (sl, _)
         | FixtureState.Classified sl -> predScoreline sl)
