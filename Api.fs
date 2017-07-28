module Api

  open System
  open System.IO
  open System.Text
  open Aether
  open Aether.Operators
  open Chiron
  open Freya.Core
  open Freya.Core.Operators
  open Freya.Machines.Http
  open Freya.Machines.Http.Cors
  open Freya.Machines.Http.Patch
  open Freya.Optics.Http
  open Freya.Routers.Uri.Template
  open Freya.Types.Http
  open Freya.Types.Http.Patch

  let guid_ : Epimorphism<string,Guid> =
    (fun x ->
      match Guid.TryParse x with
      | true, x -> Some x
      | _ -> None),
    (string)

  let inline payload () =
       function | x -> Json.parseStream x
    >> function | JPass x -> Some x
                | _ -> None
    <!> Freya.Optic.get Request.body_

  let inline represent x =
    printfn "%A" x
    { Description =
        { Charset = Some Charset.Utf8
          Encodings = None
          MediaType = Some MediaType.Json
          Languages = None}
      Data = (Inference.Json.serialize >> Encoding.UTF8.GetBytes) x }

  let id =
    Freya.memo (Option.get <!> Freya.Optic.get (Route.atom_ "id" >?> guid_))

  let add =
    Freya.memo (payload() >>= fun p -> Freya.fromAsync (p.Value |> Domain.add))

  let clear =
    Freya.memo (payload() >>= fun _ -> Freya.fromAsync (() |> Domain.clear))

  let delete =
    Freya.memo (id >>= fun i -> Freya.fromAsync (i |> Domain.delete))

  let get =
    Freya.memo (id >>= fun i -> Freya.fromAsync (i |> Domain.get))

  let list =
    Freya.memo (payload() >>= fun _ -> Freya.fromAsync (() |> Domain.list))

  let update =
    Freya.memo (id >>= fun i -> payload () >>= fun p -> Freya.fromAsync ((i, p.Value) |> Domain.update))


  let todos =
    freyaMachine {
      methods [ DELETE; GET; OPTIONS; POST ]
      created true
      doDelete (ignore <!> delete)
      doPost (ignore <!> add)
      handleCreated (represent <!> add)
      handleOk (represent <!> list)

      cors
    }

  let todo =
    freyaMachine {
      methods [ DELETE; GET; OPTIONS; PATCH ]
      doDelete (ignore <!> delete)
      handleOk (represent <!> get)

      cors

      patch
      patchDoPatch (ignore <!> update)
    }

  let todoRouter =
    freyaRouter {
      resource "/" todos
      resource "/{id}" todo
    }

  let api = todoRouter