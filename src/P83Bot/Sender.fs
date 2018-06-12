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

module P83Bot.Sender

open Chiron
open Chiron.Operators
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open System.Text

type Attachment = {Type: string; Lat: float option; Lon: float option; Name: string option; Url : string option}
                  static member ToJson (x:Attachment) =
                                 Json.write "type" x.Type
                              *> Json.writeUnlessDefault "lat" None x.Lat
                              *> Json.writeUnlessDefault "lng" None x.Lon
                              *> Json.writeUnlessDefault "name" None x.Name
                              *> Json.writeUnlessDefault "url" None x.Url


type Req = {BotId : string; Text: string; Attachments: Attachment array option}
            static member ToJson (x:Req) =
               Json.write "bot_id" x.BotId
            *> Json.write "text" x.Text
            *> Json.writeUnlessDefault "attachments" None x.Attachments


let doSend botId text attachments =
  let req = {BotId = botId; Text = text; Attachments = attachments}
            |> Json.serialize |> Json.format
  Http.AsyncRequestString
    ( "https://api.groupme.com/v3/bots/post", 
      headers = [ ContentTypeWithEncoding (HttpContentTypes.Json, Encoding.UTF8) ],
      body = TextRequest req) |> Async.Ignore

let sendMsg botId message =
  doSend botId message None

let sendLocation botId text lat lon locName =
  let attachment = {Type = "location" ; Lat = Some lat; Lon = Some lon; Name = Some locName; Url = None}
  doSend botId text (Some [|attachment|])

let sendImage botId text url =
  let attachment = {Type = "image" ; Lat = None; Lon = None; Name = None; Url = Some url}
  doSend botId text (Some [|attachment|])
  