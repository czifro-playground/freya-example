module Domain
#nowarn "0044"

  open System
  open Aether
  open Aether.Operators
  open Chiron
  open Chiron.Operators

  module E = Json.Encode
  module EI = Inference.Json.Encode
  module D = Json.Decode
  module DI = Inference.Json.Decode


  type Todo =
    { Id: Guid
      Url: string
      Order: int option
      Title: string
      Completed: bool }

    static member ToJson (x: Todo) =
      let toJson x jObj =
        jObj
        |> E.required E.guid "id" x.Id
        |> E.required E.string "url" x.Url
        |> E.optional E.int "order" x.Order
        |> E.required E.string "title" x.Title
        |> E.required E.bool "completed" x.Completed
      E.buildWith toJson x

    static member create =
      fun (newTodo:NewTodo) ->
        let id = Guid.NewGuid()

        { Id = id
          Url = sprintf "http://localhost:5000/%A" id
          Order = newTodo.Order
          Title = newTodo.Title
          Completed = false }

  and NewTodo =
    { Title: string
      Order: int option }

    static member ToJson (x: NewTodo) = Json.Null

    static member Decoder =
      (fun t o -> { Title = t; Order = o })
      <!> D.required D.string "title"
      <*> D.optional D.int "order"

    static member FromJson (_: NewTodo) = NewTodo.Decoder

  and PatchTodo =
    { Title: string
      Order: int option
      Completed: bool option }

    static member ToJson (x: PatchTodo) = Json.Null

    static member Decoder =
      (fun t o c -> { Title = t; Order = o; Completed = c })
      <!> D.required D.string "title"
      <*> D.optional D.int "order"
      <*> D.optional D.bool "completed"

    static member FromJson (_: PatchTodo) = PatchTodo.Decoder

  type State =
    { Todos: Map<Guid,Todo> }

    static member todos_ =
      (fun x -> x.Todos),(fun t x -> { x with Todos = t })

    static member empty =
      { Todos = Map.empty }

  type Protocol =
    | Add of AsyncReplyChannel<Todo> * NewTodo
    | Clear of AsyncReplyChannel<unit>
    | Delete of AsyncReplyChannel<unit> * Guid
    | Get of AsyncReplyChannel<Todo option> * Guid
    | List of AsyncReplyChannel<Todo []>
    | Update of AsyncReplyChannel<Todo> * Guid * PatchTodo

  let todos_ = State.todos_

  let todo_ id =
    todos_ >--> Map.value_ id

  let processor (mailbox: MailboxProcessor<Protocol>) =

    let reply (channel: AsyncReplyChannel<_>) x =
      printfn "reply with %A" x
      channel.Reply x
      x

    let add channel newTodo =
      Todo.create newTodo
      |> reply channel
      |> fun x -> Optic.set (todo_ x.Id) (Some x)

    let clear channel =
      ()
      |> reply channel
      |> fun _ -> Optic.set todos_ Map.empty

    let delete channel id =
      ()
      |> reply channel
      |> fun _ -> Optic.set (todo_ id) None

    let get channel id state =
      Optic.get (todo_ id) state
      |> reply channel
      |> fun _ -> state

    let list channel state =
      Optic.get todos_ state
      |> fun x -> printfn "state: %A" x; x
      |> Map.toArray
      |> Array.map snd
      |> reply channel
      |> fun _ -> state

    let update channel id patchTodo state =
      Optic.get (todo_ id) state
      |> Option.get
      |> fun x -> (function | Some t -> { x with Title = t } | _ -> x) (Some patchTodo.Title)
      |> fun x -> (function | Some o -> { x with Order = Some o } | _ -> x) patchTodo.Order
      |> fun x -> (function | Some c -> { x with Completed = c } | _ -> x) patchTodo.Completed
      |> reply channel
      |> fun x -> Optic.set (todo_ id) (Some x) state

    let rec loop (state:State) =
      printfn "State: %A" state
      async.Bind (mailbox.Receive(),
        function
          | Add (channel,newTodo) -> loop (add channel newTodo state)
          | Clear (channel) -> loop (clear channel state)
          | Delete (channel,id) -> loop (delete channel id state)
          | Get (channel,id) -> loop (get channel id state)
          | List (channel) -> loop (list channel state)
          | Update (channel,id,patchTodo) -> loop (update channel id patchTodo state))

    loop State.empty

  let state = MailboxProcessor.Start processor

  let inline fromJson (json':Json) (decoder:Decoder<JsonObject,'a>) =
    match json' with
    | Object x ->
      match decoder x with
      | JPass v -> v
      | _ -> failwith "could not parse"
    | _ -> failwith "wrong type"

  let add (newTodoJson:Json) =
    let newTodo = fromJson newTodoJson NewTodo.Decoder
    state.PostAndAsyncReply (fun channel -> Add (channel, newTodo))

  let clear () =
    state.PostAndAsyncReply (fun channel -> Clear (channel))

  let delete id =
    state.PostAndAsyncReply (fun channel -> Delete (channel, id))

  let get id =
    state.PostAndAsyncReply (fun channel -> Get (channel, id))

  let list () =
    state.PostAndAsyncReply (fun channel -> List (channel))

  let update (id,patchTodoJson) =
    let patchTodo = fromJson patchTodoJson PatchTodo.Decoder
    state.PostAndAsyncReply (fun channel -> Update (channel, id, patchTodo))
