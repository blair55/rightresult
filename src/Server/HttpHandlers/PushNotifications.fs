namespace Server.HttpHandlers

open System
open FSharp.Data
open WebPush
open Shared

module PushNotifications =

  type PushKeys = {
    Subject : string
    Public : string
    Private : string }

  let toPushSub (push:Shared.PushSubscription) =
    new WebPush.PushSubscription(push.Endpoint, push.Keys.P256dh, push.Keys.Auth)

  let send (keys:PushKeys) message (push:Shared.PushSubscription) =
    let client = WebPush.WebPushClient ()
    let vapidKeys = WebPush.VapidDetails (keys.Subject, keys.Public, keys.Private)
    try
      client.SendNotification (toPushSub push, message, vapidKeys)
    with ex ->
      printfn "%s" ex.Message
