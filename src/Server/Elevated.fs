namespace Server.Elevated

open System.Threading.Tasks

module Async =

  let retn x = async {
    return x }

  let map f xAsync = async {
    let! x = xAsync
    return f x }

  let bind f xAsync = async {
    let! x = xAsync
    return! f x }

  let toTask f a =
    Async.StartAsTask (f a)

module Result =

  let retn =
    Ok

  let bind f xResult =
    match xResult with
    | Ok x -> f x
    | Error errs -> Error errs

  let apply f xResult =
    match f, xResult with
    | Ok f, Ok x -> Ok (f x)
    | Error errs, _ -> Error errs
    | _, Error errs -> Error errs

  let (<*>) = apply
  let (<!>) = Result.map

  let lift2 f x y =
      f <!> x <*> y

  let traverseResultM f list =

    // define the monadic functions
    let (>>=) x f = Result.bind f x

    // define a "cons" function
    let cons head tail =
      head :: tail

    // right fold over the list
    let folder head tail =
      f head >>= (fun h -> tail >>= (fun t -> retn (cons h t)))

    List.foldBack folder list (retn [])

module AsyncResult =

  let retn x =
    x |> Result.retn |> Async.retn

  let map f =
    f |> Result.map |> Async.map

  let mapError f =
    f |> Result.mapError |> Async.map

  let apply fAsyncResult xAsyncResult =
    fAsyncResult |> Async.bind (fun fResult ->
    xAsyncResult |> Async.map (fun xResult ->
    Result.apply fResult xResult))

  let bind f xAsyncResult = async {
    let! xResult = xAsyncResult
    match xResult with
    | Ok x -> return! f x
    | Error err -> return (Error err) }

  let AwaitTask (t:Task) = Async.AwaitTask t |> Async.map Result.retn

  let AwaitTaskOf (t:Task<'a>) = Async.AwaitTask t |> Async.map Result.retn


  type AsyncResultBuilder() =
      member __.Return(x) = retn x
      member __.ReturnFrom(m: Async<Result<_, _>>) = m
      member __.Bind(m, f) = bind f m
      member __.Zero() = Error()

  let asyncResult = AsyncResultBuilder()

module Task =

  let retn x = task {
    return x }

  let map f (xTask:Task<'a>) = task {
    let! x = xTask
    return f x }

  let bind f (xTask:Task<'a>) = task {
    let! x = xTask
    return! f x }

  let toAsync t =
    Async.AwaitTask t

[<AutoOpen>]
module ElevatedOperators =
  let (>>=) = Result.bind