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

module P83Bot.BotHandler

open FSharp.Control
open FSharp.Data
open NodaTime
open System
open System.Globalization

type Response =
  | Text of string
  | Location of float * float * string
  | Image of string

let swayzeImage = "https://i.groupme.com/900x750.jpeg.726f02cbaf0a4599b9282de88c77aebb"

let helpText = "Sample commands:
@point83 rule <n>
@point83 nyc rule <n>
@point83 wait <num seconds>"

let asyncToAsyncSeq a = asyncSeq {
   let! r = a
   yield r
}

let (|Prefix|_|) (p:string) (s:string) =
  if s.ToLower().StartsWith(p.ToLower()) then Some(s.Substring(p.Length)) else None

let (|Contains|) (p:string) (s:string) =
  s.ToLower().Contains(p.ToLower())

let (|IgnoreCase|) (p:string) (s:string) =
  s.Equals(p, StringComparison.InvariantCultureIgnoreCase)

let (|TryToInt|_|) s =
  match System.Int32.TryParse s with
  | true, v -> Some v
  | false, _ -> None

let (|TryToFloat|_|) s =
  match System.Double.TryParse s with
  | true, v -> Some v
  | false, _ -> None

let getRule (ruleSet : Async<string list * Instant>) text = asyncSeq {
  let! r = ruleSet |> Async.map fst
  yield (match text with
         | TryToInt n when (n > 0 && n <= r.Length)
             -> (List.item (n-1) r) |> (fun r -> if r = "" then "<blank>" else r)
         | _ -> "no such rule...") |> Text
}

let staleness () = asyncSeq {
  let now = SystemClock.Instance.GetCurrentInstant()
  let! r = Rules.rules() |> Async.map (fun x -> (now - snd x).ToString("%M", CultureInfo.InvariantCulture))
  let! nyc = Rules.rules() |> Async.map (fun x -> (now - snd x).ToString("%M", CultureInfo.InvariantCulture))
  yield sprintf "General rules refreshed: %s mins ago. NYC rules refreshed: %s mins ago" (r.ToString()) (nyc.ToString()) |> Text
}

let randomLoc () = asyncSeq {
  let rnd = Random()
  let locs = [(40.7195654,-73.9457874, "hell"); (40.6593619,-73.9878565, "Greenwood");
              (40.6762108,-73.9835355,"misson dolores"); (40.7435403,-73.9536081,"Dominie's Hoek")]
  let first = List.item (rnd.Next(locs.Length)) locs
  yield first |> Location
  do! Async.Sleep 10000
  let second = List.item (rnd.Next(locs.Length)) locs
  if first <> second then
    yield Text "On second thought, how about here:"
    yield second |> Location
}

let wait t = asyncSeq {
  match t with
  | TryToFloat n -> yield Text (sprintf "Going to wait %g seconds" n)
                    do! Async.Sleep (Convert.ToInt32(n*1000.0))
                    yield Text (sprintf "waited %g seconds" n)
  | _ -> yield Text "not a valid amount"
}

let nextRide () =
  let day = IsoDayOfWeek.Thursday
  let time = LocalTime.FromHourMinuteSecondTick (19, 30, 0, 0)
  let now = SystemClock.Instance.GetCurrentInstant()
  let zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull("America/New_York")
  let localNow = now.InZone(zone)
  if localNow.DayOfWeek = day && localNow.TimeOfDay < time then
    let until = (localNow.LocalDateTime - (localNow.LocalDateTime.Date + time)).ToDuration()
    let hrs = until.ToString("%H", CultureInfo.InvariantCulture)
    let mins = until.ToString("%m", CultureInfo.InvariantCulture)
    sprintf "Today in %s hrs %s mins" hrs mins
  else
    let next = now.InUtc().LocalDateTime.Date.Next(day)
    (next + time).ToString("ddd d MMM h:mm tt", CultureInfo.InvariantCulture)

let uptime =
  let startupTime = SystemClock.Instance.GetCurrentInstant()
  fun () -> (SystemClock.Instance.GetCurrentInstant() - startupTime)
                .ToString("%HH:%mm", CultureInfo.InvariantCulture)

let parseAndReply text sender isBot =
  match text with
  | Prefix("@point83 ") rest ->
      match rest with
      | Prefix("rule ") rule -> getRule (Rules.rules()) rule
      | Prefix("nyc rule ") rule -> getRule (Rules.nycRules()) rule
      | IgnoreCase("staleness") true -> staleness()
      | IgnoreCase("jeff's bike count") true -> Text "3" |> asyncSeq.Yield
      | IgnoreCase("help") true -> Text helpText  |> asyncSeq.Yield
      | IgnoreCase("next ride") true -> nextRide () |> Text |> asyncSeq.Yield
      | IgnoreCase("location") true -> (40.718978, -73.993025, "home") |> Location |> asyncSeq.Yield
      | IgnoreCase("somewhere") true -> randomLoc ()
      | IgnoreCase("swayze") true -> Image swayzeImage |> asyncSeq.Yield
      | IgnoreCase("uptime") true -> uptime () |> Text |> asyncSeq.Yield
      | Prefix("wait") w -> wait w
      | _ -> Text "Huh?" |> asyncSeq.Yield
  | Contains(" R Bar") true when not isBot -> sprintf "@%s R Bar is dead. Long live R Bar!" sender |> Text |> asyncSeq.Yield
  | Contains("Here's a pun:") true -> "shut the fuck up" |> Text |> asyncSeq.Yield
  | _ -> asyncSeq.Zero ()
