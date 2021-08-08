module Server.Infrastructure.Documents

open Shared
open Server.Elevated

let mutable _ds : Map<Document, obj> =
  Map.empty

let private readDocument<'a> () document =
  _ds.TryFind document |> Option.map (fun a -> a :?> 'a)

let private storeDocument<'a> () document (a:'a) =
  _ds <- _ds.Add(document, a :> obj)

let private deleteDocument () document =
  _ds <- _ds.Remove(document)

let private upsertDocument client document (init:'a) (f:'a -> 'a) =
  match readDocument client document with
  | Some d -> d
  | None -> init
  |> (f >> storeDocument client document)

let private editDocument client document (f:'a -> 'a) =
  match readDocument client document with
  | Some d -> d |> (f >> storeDocument client document) |> Ok
  | None -> RemoteError.ServerSideError "Could not find document to edit" |> Result.Error

type Repo<'d> =
  { Read : Document -> 'd option
    Edit : Document -> ('d -> 'd) -> Rresult<unit>
    Upsert : Document -> 'd -> ('d -> 'd) -> unit
    Insert : Document -> 'd -> unit
    Delete : Document -> unit
    Print : unit -> string
  }

let repo ds =
  { Read = readDocument ds
    Edit = editDocument ds
    Upsert = upsertDocument ds
    Insert = storeDocument ds
    Delete = deleteDocument ds
    Print = fun () -> _ds |> Json.srlzToString
  }
