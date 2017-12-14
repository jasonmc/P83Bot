module P83Bot.Tests

open FSharp.Control
open P83Bot
open NUnit.Framework

[<Test>]
let ``nyc rules work`` () =
  let resp = BotHandler.parseAndReply "@point83 nyc rule 1" "jason" false
  let r = resp |> AsyncSeq.toList
  Assert.AreEqual([BotHandler.Text "The regular basic rules apply, except where they don't."], r)
  ()

[<Test>]
let ``don't reply to bots`` () =
  let resp = BotHandler.parseAndReply "R Bar something" "jason" true
  let r = resp |> AsyncSeq.toList
  Assert.IsEmpty r
  ()
