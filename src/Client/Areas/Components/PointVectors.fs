namespace Areas.Components

open Fable.React
open Fable.React.Props
open Fulma

open Shared

module PointVectors =

  let private row class' left right =
    div [ Class class' ] [
      Columns.columns [ Columns.IsMobile
                        Columns.IsGapless
                        Columns.Props [ Props.Style [ MarginBottom "0" ] ] ] [
        Column.column
          [ Column.Modifiers [ Modifier.FlexDirection FlexDirection.RowReverse ]
            Column.Width(Screen.All, Column.IsHalf) ]
          left
        Column.column
          [ Column.Modifiers [ Modifier.FlexJustifyContent FlexJustifyContent.Left ]
            Column.Width(Screen.All, Column.IsHalf) ]
          right
      ]
    ]

  // let waysToSayZero =
  //   [ "zero"; "zip"; "didley"; "nada"; "zilch"; "squat"; "nuthin"; "bumpkis"; "nil"; "" ]

  let (|VectorDesc|DoubleDownDesc|) =
    function
    | PointVector.Result -> VectorDesc(str "Right Result", pluralPoints 2)
    | PointVector.HomeScore -> VectorDesc(str "Home Score", pluralPoints 1)
    | PointVector.AwayScore -> VectorDesc(str "Away Score", pluralPoints 1)
    | PointVector.GoalDifference -> VectorDesc(str "Goal Difference", pluralPoints 1)
    | PointVector.BigUp b -> VectorDesc(str "Big Up", pluralPoints b)
    | PointVector.DoubleDown -> DoubleDownDesc(str "Double Down")

  let vector =
    function
    | VectorDesc (desc, p) ->
      row
        "point-vector-row"
        [ div [ Class "point-vector-row-left" ] [
            desc
          ] ]
        [ div [ Class "point-vector-row-right" ] [
            str "+ "
            p
          ] ]
    | DoubleDownDesc desc ->
      row
        "point-vector-row"
        [ div [ Class "point-vector-row-left" ] [
            desc
          ] ]
        [ div [ Class "point-vector-row-right" ] [
            str "× 2"
          ] ]

  let totalPoints p =
    row
      "point-vector-row-total"
      []
      [ div [ Class "point-vector-row-right" ] [
          pluralPoints p
        ] ]

  let element (p, vectors) =
    div [ Class "point-vectors" ] [
      div [ Class "point-vectors-body" ] (List.map vector vectors)
      totalPoints p
    ]
