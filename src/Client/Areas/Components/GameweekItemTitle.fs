namespace Areas.Components

open Fable.React
open Fable.React.Props
open Shared

module GameweekItemTitle =

  let element (ko: KickOffComponents, TeamLine (h, a)) =

    div [ Class "gw-item-title" ] [
      div [ Style [ Display DisplayOptions.Flex ] ] [
        div [ Class "gw-item-gameday" ] [
          str ko.ShortDay
          str " "
          str ko.ClockTime
        ]
        div [ Class "gw-item-team" ] [
          teamName h
          span [ Class "gw-item-v" ] [ str " v " ]
          teamName a
        ]
      ]
      div [ Class "gw-item-date" ] [
        str (ko.DateAndShortMonth)
      ]
    ]
