namespace Areas.Components

open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma
open Browser.Dom
open Areas

module SubMenu =

  let subMenuId = "submenu"
  let hideSubMenuClass = "hide-submenu"
  let isActiveClass = "is-active"

  let mutable prevScrollPos = window.pageYOffset

  let dropdownElem (e: Browser.Types.HTMLElement) =
    e.getElementsByClassName("dropdown").Item(0)

  document.onscroll <-
    fun _ ->
      let currentScrollPos = window.pageYOffset

      match Html.tryElemById subMenuId with
      | Some e when prevScrollPos > currentScrollPos ->
        e.classList.remove hideSubMenuClass
        (dropdownElem e).classList.remove isActiveClass
      | Some e ->
        e.classList.add hideSubMenuClass
        (dropdownElem e).classList.remove isActiveClass
      | _ -> ()

      prevScrollPos <- currentScrollPos

  // Html.elemById subMenuId
  // |> dropdownElem
  // |> fun e -> e.classList.add "is-fullwidth"
  // document.onclick <-
  //   fun _ ->
  //     match Html.tryElemById subMenuId with
  //     | Some e ->
  //         dropdownElem e
  //         |> fun e -> e.classList.remove isActiveClass
  //     | None -> ()

  let toggle _ =
    Html.elemById subMenuId
    |> dropdownElem
    |> fun e -> e.classList.toggle isActiveClass
    |> ignore

  // let remove (e:Browser.Types.FocusEvent) =
    // toggle ()
    // e.stopPropagation()
    // Html.elemById subMenuId
    // |> dropdownElem
    // |> fun e -> e.classList.remove isActiveClass

  let menulink nav (icon, text, route) =
    Dropdown.Item.a [ Dropdown.Item.Props(anchorNavProps nav route) ] [
      span [ Class "mr-2" ] [
        Fa.i [ icon ] []
      ]
      str text
    ]

  let buildLinkMenu nav links =
    links
    |> List.rev
    |> List.mapi
         (fun i l ->
           menulink nav l
           :: (if i = (List.length links) - 1 then
                 []
               else
                 [ Dropdown.divider [] ]))
    // |> List.collect
    //      (fun l ->
    //        [ menulink nav l
    //          Dropdown.divider [] ])
    |> List.concat

  let width100 = Style [ Width "100%" ]

  let element nav links =
    div [] [

      div [ Id subMenuId ] [
        Text.div [ Modifiers [ Modifier.IsHidden(Screen.Desktop, true) ] ] [
          Dropdown.dropdown [ Dropdown.IsUp
                              Dropdown.Props [ OnClick toggle
                                              //  OnBlur remove
                                               width100 ] ] [
            Dropdown.trigger [ Props [ width100 ] ] [
              Button.button [ Button.IsFullWidth
                              Button.Color IsDark
                              Button.IsOutlined
                              Button.IsFocused true ] [
                span [] [ str "League Links" ]
                Icon.icon [ Icon.Size IsSmall ] [
                  Fa.i [ Fa.Solid.AngleRight ] []
                ]
              ]
            ]
            Dropdown.menu [ Props [ width100 ] ] [
              Dropdown.content [] (buildLinkMenu nav links)
            ]
          ]
        ]
      ]
      Card.card [ Modifiers [ Modifier.IsHidden(Screen.Touch, true)
                              Modifier.Spacing(Spacing.MarginTop, Spacing.Amount.Is6) ] ] [
        Panel.panel
          [ Panel.Color IsPrimary ]
            (List.map (fun (icon, text, route) -> panelAnchor icon text nav route) links |> List.rev)
      ]
    ]
