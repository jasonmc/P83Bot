(*  Copyright (C) 2017 Jason McCandless

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. *)
    
module P83Bot.Main

open BotHandler
open FSharp.Configuration
open FSharp.Control
open FSharp.Data
open Suave
open Suave.Operators
open Suave.Filters
open System

type Callback = JsonProvider<"""{
"attachments": [],
"avatar_url": "https://i.groupme.com/123456789",
"created_at": 1302623328,
"group_id": "1234567890",
"id": "1234567890",
"name": "John",
"sender_id": "12345",
"sender_type": "user",
"source_guid": "GUID",
"system": false,
"text": "Hello world ☃☃",
"user_id": "1234567890"
}""">

type Config = YamlConfig<"config.yaml">
let config = Config()


let startAsync r = Async.Start (async {
  try return! r
  with ex ->
    printfn "%A" ex
    return ()
})

let send botId = function
  | Response.Text t -> Sender.sendMsg botId t
  | Location (lat, lon, name) -> Sender.sendLocation botId "" lat lon name
  | Image url -> Sender.sendImage botId "" url

let callback botId req = 
    let req = (System.Text.Encoding.ASCII.GetString req.rawForm) |> Callback.Parse
    let r = parseAndReply req.Text req.Name (req.SenderType = "bot")
    r |> AsyncSeq.iterAsync (send botId) |> startAsync
    Successful.OK "done"

let rec doPeriodicWork f (ts: TimeSpan) = async {
  do! f()
  do! Async.Sleep (int (ts.TotalMilliseconds))
  return! doPeriodicWork f ts
}

let app : WebPart =
  choose [
    path "/callback" >=> POST >=> request (callback config.GroupMe.ReadlBotId)
    path "/testcallback" >=> POST >=> request (callback config.GroupMe.TestBotId)
  ]

[<EntryPoint>]
let main _ =
  doPeriodicWork Rules.refresher (TimeSpan.FromSeconds 5.0) |> startAsync
  let serverConfig =
    { defaultConfig with
       bindings = [ HttpBinding.create HTTP Net.IPAddress.Any 7883us ]
    }
  "Starting up" |> Sender.sendMsg config.GroupMe.TestBotId |> Async.RunSynchronously  
  startWebServer serverConfig app
  0