module Server.Infrastructure.Push

open System.Text
open Shared

type PushKeys =
  { Subject : string
    Public : string
    Private : string }

type PushMessage =
  { Title : string
    Body : string }

type PushNotify =
  PushMessage -> PushSubscription -> Unit

let mutable semaphore =
  false

let send (keys:PushKeys) (message:PushMessage) (push:PushSubscription) =
  if semaphore then
    try
      let client = new WebPush.WebPushClient ()
      let vapidKeys = WebPush.VapidDetails (keys.Subject, keys.Public, keys.Private)
      let ps = new WebPush.PushSubscription (push.Endpoint, push.Keys.P256dh, push.Keys.Auth)
      client.SendNotification (ps, Json.srlzToString message, vapidKeys)
    with ex ->
      printfn "%s" ex.Message
