module Program

open KestrelInterop
open Freya.Core
open Suave
open Suave.Owin
open Suave.Logging

let app = ApplicationBuilder.useFreya Api.api

let useKestrel() =
  WebHost.create ()
  |> WebHost.bindTo [|"http://localhost:5000"|]
  |> WebHost.configure app
  |> WebHost.buildAndRun

[<EntryPoint>]
let main argv =
  useKestrel()
  0
