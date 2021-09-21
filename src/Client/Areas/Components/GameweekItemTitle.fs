namespace Areas.Components

open Fable.React
open Fable.React.Props
open Shared

module GameweekItemTitle =

  let element (ko:KickOffComponents, TeamLine (h, a)) =

    div [ Class "gw-item-title" ] [
      div [ Class "gw-item-gameday" ] [
        str ko.ShortDay
      ]
      div [ Class "gw-item-team" ] [
        teamName h
      ]
      div [ Class "gw-item-ko" ] [
        str ko.ClockTime
      ]
      div [ Class "gw-item-team" ] [
        teamName a
      ]
    ]
