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

module P83Bot.Rules

open FSharp.Data
open FSharp.Data.Runtime.Caching
open NodaTime
open System

let url = "http://point83.com/tos/index.php?title=Basic_rules"
let nycUrl = "http://point83.com/tos/index.php?title=Basic_Rules_(NYC_Addendum)"

let getRuleset url = async {
  let! x =  HtmlDocument.AsyncLoad url
  let text = x.Descendants["ol"] |> Seq.head |> (fun x -> x.Elements())
             |> Seq.map HtmlNode.innerText |> List.ofSeq
  return text, SystemClock.Instance.GetCurrentInstant()
}

let cacheIt =
  let cache = createInMemoryCache (TimeSpan.FromMinutes 15.0)
  fun f key -> match cache.TryRetrieve key with
               | Some value -> async.Return value
               | None -> f () |> Async.map (fun x -> cache.Set(key, x)
                                                     x)

let rules =
  let key = Guid.NewGuid().ToString()
  fun () -> cacheIt  (fun () -> getRuleset url) key

let nycRules =
  let key = Guid.NewGuid().ToString()
  fun () -> cacheIt (fun () -> getRuleset nycUrl) key

let refresher () =
    Async.Parallel [rules(); nycRules()] |> Async.Ignore
