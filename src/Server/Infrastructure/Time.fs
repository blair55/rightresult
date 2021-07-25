module Server.Infrastructure.Time

open System

let private tz =
  TimeZoneInfo.FindSystemTimeZoneById("Europe/London")

let toUkTime (date:DateTime) =
  tz.GetUtcOffset date
  |> date.Add
  |> fun d -> new DateTimeOffset(d)
