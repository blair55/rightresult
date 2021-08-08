namespace Areas.Players

open Elmish

open Fable.React
open Fable.React.Props

open Areas
open Shared
open Fulma
open Fable
open Routes
open Elmish.React
open Fable.FontAwesome
open Areas.Components

module MyProfile =

  type Model =
    { Player: PlayerViewModel WebData
      PointsTotal: PredictionPointsMonoid WebData }

  type Msg =
    | Init of Result<string, exn>
    | NavTo of Route
    | PlayerReceived of Rresult<PlayerViewModel>
    | PlayerPointsTotalReceived of Rresult<PredictionPointsMonoid>
    | Logout

  let init api p =
    { Player = Fetching
      PointsTotal = Fetching },
    Cmd.batch [ Cmd.OfAsync.either api.getMyProfile p.Token PlayerReceived (Error >> Init)
                Cmd.OfAsync.either api.getMyPointsTotal p.Token PlayerPointsTotalReceived (Error >> Init) ]

  let pointsTotalBar ({ PredictionPointsMonoid.Points = total } as p) =
    div [ Class "me-total-points-bar" ] [
      Box.box' [] [
        Components.pointsTotalView p
      ]
    ]

  let badgesBar () =
    div [ Class "me-badges-bar" ] [
      Components.subHeading "Badges"

      Message.message [ Message.Color IsWarning ] [
        Message.body [] [
          p [] [
            str "Badges are still under construction but please let us know your ideas for more!"
          ]
          p [] [
            a [ Href(Components.Social.twitterHref "@RightResu_lt") ] [
              Fa.i [ Fa.Brand.Twitter ] []
              str "twitter"
            ]
          ]
          p [] [
            a [ Href("https://www.facebook.com/rightresultrightresult") ] [
              Fa.i [ Fa.Brand.FacebookSquare ] []
              str "facebook"
            ]
          ]
        ]
      ]

      let badgeBox icon title desc =
        Message.message [ Message.Color IsInfo ] [
          Message.body [] [
            div [ Class "me-badge-item" ] [
              span [] [ Fa.i [ Fa.Size Fa.FaLarge; icon ] [] ]
              span [ Class "me-badge-title" ] [
                str title
              ]
              p [] [ str desc ]
            ]
          ]
        ]

      div [ Class "me-badge-list" ] [
        badgeBox Fa.Solid.Award "Gameweek Winner" "Win a gameweek in the Global League"
        badgeBox Fa.Solid.Award "Month Winner" "Win a month in the Global League"
        badgeBox Fa.Solid.Award "Nail a Double Down" "Get correct score when using Double Down"
        badgeBox Fa.Solid.Award "Nail a Big Up" "Get correct score when using Big Up"
        badgeBox Fa.Solid.Award "Full House" "Get correct score for every team"
        badgeBox Fa.Solid.Award "Bore Draw" "Correctly predict a 0-0"
        badgeBox Fa.Solid.Award "One All Face" "Correctly predict a 1-1"
        badgeBox Fa.Solid.Award "A Desmond" "Correctly predict a 2-2"
        badgeBox Fa.Solid.Award "Thriller" "Correctly predict a 3-3"
        badgeBox Fa.Solid.Award "Goal Fest" "Get correct score in fixture with 5 goals or more"
        badgeBox Fa.Solid.Award "Good Shout" "Get correct score when 75% of players get the wrong result"
        badgeBox Fa.Solid.Award "Great Shout" "Get correct score when 95% of players get the wrong result"
        badgeBox Fa.Solid.Award "Shoot the Moon" "Predict the wrong result on every fixture in a gameweek"
        badgeBox Fa.Solid.Award "Shoot Uranus" "Predict on every fixture and get zero points in a gameweek"
      ]

    ]

  let logoutButton dispatch =
    Box.box' [] [
      div [ Class "block" ] [
        Button.button
          ([ Button.IsFullWidth
             Button.IsOutlined
             Button.Color IsInfo
             Button.Color IsLight
             Button.OnClick(fun _ -> Logout |> dispatch) ])
          [ str "Log Out" ]
      ]
    ]

  let fullView dispatch ({ Name = PlayerName name } as vm: PlayerViewModel) points =
    div [ Class "me-page" ] [
      Components.pageTitle name
      pointsTotalBar points
      badgesBar ()
      logoutButton dispatch
    ]

  let view (model: Model) dispatch =
    match model.Player, model.PointsTotal with
    | Success player, Success points -> fullView dispatch player points
    | _ -> div [] []

  let update api player msg model : Model * Cmd<Msg> =
    match msg with
    | Init _ -> model, []
    | Logout -> model, []
    | NavTo r -> model, (Routes.navTo r)
    | PlayerReceived r ->
      { model with
          Player = resultToWebData r },
      []
    | PlayerPointsTotalReceived r ->
      { model with
          PointsTotal = resultToWebData r },
      []
